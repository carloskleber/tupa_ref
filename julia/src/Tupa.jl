module Tupa
using FFTW, JSON3, LinearAlgebra, Plots, Printf, SpecialFunctions
export Study, load_study, prepare!, solve_frequency, run_sweep, run_file,
       transient_response, tukey_antialias, sample_time_axis,
       one_sided_frequency_axis, heidler, double_exponential,
       write_transient_plot
const EPS0=8.8541878128e-12; const MU0=1.25663706212e-6; const DEFAULT_TUKEY_ALPHA=.75

struct Material
    id::String; kind::Symbol; epsilonr::Float64; mur::Float64; sigma::Float64; alpha0::Float64; kr::Float64
end
function admittance(m::Material,w)
    m.kind===:linear && return complex(m.sigma,w*m.epsilonr*EPS0)
    m.kind===:portela && return complex(m.sigma,0)+m.kr*complex(cot(pi*m.alpha0/2),1)*(w/(2pi*1e6))^m.alpha0
    f=w/(2pi); xi=.54; h=1.26*(1e3*m.sigma)^(-.73); ds=m.sigma*h*(f/1e6)^xi
    complex(m.sigma+ds,w*EPS0*12+ds*tan(pi*xi/2))
end
struct Segment
    id::String; n1::Int; n2::Int; p1::Vector{Float64}; p2::Vector{Float64}; radius::Float64; material::Material; medium::Int
end
mutable struct Study
    title::String; soil::Material; air::Material; nodes::Vector{Pair{String,Vector{Float64}}}
    segments::Vector{Segment}; elements::Vector{Any}; prepared::Bool; geom::NamedTuple
end
Study(title,soil)=Study(title,soil,Material("air",:linear,1,1,0,0,0),Pair{String,Vector{Float64}}[],Segment[],Any[],false,(;))
getv(x,k,d=nothing)=haskey(x,k) ? x[k] : d
asvec(x)=Float64[x...]
nodeindex(s,id)=something(findfirst(p->p.first==id,s.nodes),0)
segmentindex(s,id)=something(findfirst(p->p.id==id,s.segments),0)
function material_from_json(x;id="soil")
    kind=Symbol(replace(String(getv(x,:type,"linear")),"-"=>"_")); kind===:alipio_visacro && (kind=:alipio)
    Material(id,kind,Float64(getv(x,:permittivity,getv(x,:epsilonr,0))),
      Float64(getv(x,:permeability,getv(x,:mur,1))),
      Float64(getv(x,:conductivity,getv(x,:sigma,getv(x,:sigma0,0)))),
      Float64(getv(x,:alpha0,0)),Float64(getv(x,:kr,0)))
end
"Load the same v1 study JSON accepted by the reference Fortran program."
function load_study(path::AbstractString)
    root=JSON3.read(read(path,String)); s=Study(String(getv(root,:title,"Untitled")),material_from_json(root[:soil])); mats=Dict{String,Material}()
    for m in getv(root,:materials,()); mm=material_from_json(m;id=String(m[:id])); mats[mm.id]=mm; end
    for n in getv(root,:nodes,()); push!(s.nodes,String(n[:id])=>asvec(n[:position])); end
    for e in getv(root,:elements,()); push!(s.elements,(type=String(e[:type]),data=e,materials=mats)); end
    assemble!(s); s,root
end
function addnode!(s,id,p)
    i=nodeindex(s,id); i>0 && return i; push!(s.nodes,id=>Float64[p...]); length(s.nodes)
end
function addline!(s,id,from,to,radius,nseg,mat)
    i1=nodeindex(s,from); i2=nodeindex(s,to); (i1==0||i2==0) && error("line $id references an unknown node")
    a=s.nodes[i1].second; b=s.nodes[i2].second
    ((a[3]<0)!=(b[3]<0)||a[3]==0||b[3]==0) && error("line $id crosses/touches the air-soil interface")
    prev=i1
    for k=1:nseg
        nxt=k==nseg ? i2 : addnode!(s,"$(id)_n$k",a+(b-a)*(k/nseg)); p1=s.nodes[prev].second; p2=s.nodes[nxt].second
        push!(s.segments,Segment("$(id)_e$k",prev,nxt,p1,p2,radius,mat,p1[3]>0 ? 1 : 2)); prev=nxt
    end
end
function assemble!(s)
    isempty(s.segments)||return s
    for item in s.elements
        e=item.data; mat=item.materials[String(e[:material])]
        if item.type=="line"
            addline!(s,String(e[:id]),String(e[:from]),String(e[:to]),Float64(e[:radius]),Int(e[:segments]),mat)
        elseif item.type=="mesh"
            id=String(e[:id]); o=asvec(e[:position]); nx=Int(e[:rowsX]); ny=Int(e[:rowsY]); lx=Float64(e[:lengthX]); ly=Float64(e[:lengthY]); ns=Int(e[:segments]); r=Float64(e[:radius])
            ids=[@sprintf("%s-%02d%02d",id,i-1,j-1) for i=1:nx,j=1:ny]
            for i=1:nx,j=1:ny; addnode!(s,ids[i,j],o+[lx*(j-1)/(ny-1),ly*(i-1)/(nx-1),0]); end
            for i=1:nx,j=1:ny-1; addline!(s,"$(id)_x$(i)_$(j)",ids[i,j],ids[i,j+1],r,ns,mat); end
            for j=1:ny,i=1:nx-1; addline!(s,"$(id)_y$(i)_$(j)",ids[i,j],ids[i+1,j],r,ns,mat); end
        else; @warn "unknown element type; skipped" type=item.type; end
    end; s
end

segdata(q)=begin d=q.p2-q.p1;l=norm(d);(d/l,l,(q.p1+q.p2)/2) end
selfgeom(l,r)=2*(l*log((l+hypot(l,r))/r)-hypot(l,r)+r)
image(p)=[p[1],p[2],-p[3]]
function mutualgeom(a,b;rtol=1e-6)
    va,la,_=segdata(a); vb,lb,_=segdata(b)
    # The endpoint singularity of connected conductors is integrable. A
    # tensor midpoint rule never samples that singular point and is both
    # deterministic and much faster than deeply nested adaptivity there.
    order=64; dx=la/order; dy=lb/order; total=0.0
    for i=1:order,j=1:order
        total += inv(norm((a.p1+(i-.5)*dx*va)-(b.p1+(j-.5)*dy*vb)))
    end
    total*dx*dy
end
function prepare!(s;rtol=1e-6)
    s.prepared&&return s; n=length(s.segments); G=zeros(n,n);Gi=zeros(n,n);R=zeros(n,n);Ri=zeros(n,n);C=zeros(n,n);Ci=zeros(n,n);L=zeros(n)
    for i=1:n
        a=s.segments[i];va,la,ma=segdata(a);L[i]=la
        for j=i:n
            b=s.segments[j];vb,lb,mb=segdata(b); mixed=i!=j&&a.medium!=b.medium
            if i==j; G[i,i]=selfgeom(la,a.radius);R[i,i]=a.radius;C[i,i]=1
            elseif !mixed; G[i,j]=G[j,i]=mutualgeom(a,b;rtol);R[i,j]=R[j,i]=norm(ma-mb);C[i,j]=C[j,i]=dot(va,vb);end
            if !mixed
                bi=Segment(b.id,b.n1,b.n2,image(b.p1),image(b.p2),b.radius,b.material,b.medium)
                Gi[i,j]=Gi[j,i]=mutualgeom(a,bi;rtol);Ri[i,j]=Ri[j,i]=norm(ma-image(mb));Ci[i,j]=Ci[j,i]=dot(va,image(vb))
            end
        end
    end
    s.geom=(G=G,Gi=Gi,R=R,Ri=Ri,C=C,Ci=Ci,L=L);s.prepared=true;s
end
function internal_impedance(q,w,l)
    m=q.material;rho=q.radius*sqrt(im*w*m.mur*MU0*m.sigma);ratio=abs(rho)>500 ? one(rho) : besseli(0,rho)/besseli(1,rho)
    sqrt(im*w*m.mur*MU0/m.sigma)/(2pi*q.radius)*ratio*l
end
"Solve HEM at angular frequency omega; return node voltage and end currents."
function solve_frequency(s,omega,source_ids,source_values)
    prepare!(s);nn=length(s.nodes);ns=length(s.segments);g=s.geom
    A=zeros(ComplexF64,ns,nn);B=zeros(ComplexF64,ns,nn);C=zeros(ComplexF64,nn,ns);D=zeros(ComplexF64,nn,ns)
    for (i,q) in pairs(s.segments);A[i,q.n1]=-1;A[i,q.n2]=1;B[i,q.n1]=B[i,q.n2]=-.5;C[q.n1,i]=1;D[q.n2,i]=1;end
    wa=admittance(s.air,omega);ws=admittance(s.soil,omega)
    pars=((1/(4pi*wa),im*omega*s.air.mur*MU0/(4pi),sqrt(im*omega*s.air.mur*MU0*wa),-1.),(1/(4pi*ws),im*omega*s.soil.mur*MU0/(4pi),sqrt(im*omega*s.soil.mur*MU0*ws),1.))
    Zt=zeros(ComplexF64,ns,ns);Zl=similar(Zt)
    for i=1:ns,j=i:ns
        a=s.segments[i];b=s.segments[j];a.medium==b.medium||continue;ce,cm,prop,sgn=pars[a.medium];fp=exp(-g.R[i,j]*prop);fpi=exp(-g.Ri[i,j]*prop)
        Zt[i,j]=ce*(fp*g.G[i,j]+sgn*fpi*g.Gi[i,j])/(g.L[i]*g.L[j]);Zl[i,j]=cm*(g.C[i,j]*fp*g.G[i,j]+sgn*g.Ci[i,j]*fpi*g.Gi[i,j])
        i==j&&(Zl[i,j]+=internal_impedance(a,omega,g.L[i]));Zt[j,i]=Zt[i,j];Zl[j,i]=Zl[i,j]
    end
    Z=[A Zl/2 -Zl/2;B Zt Zt;zeros(ComplexF64,nn,nn) C D];rhs=zeros(ComplexF64,nn+2ns)
    for (id,v) in zip(source_ids,source_values);p=nodeindex(s,String(id));p>0||error("unknown source node $id");rhs[2ns+p]+=v;end
    x=Z\rhs;x[1:nn],x[nn+1:nn+ns],x[nn+ns+1:end]
end
function run_sweep(s,freqs,ids,values)
    V=Matrix{ComplexF64}(undef,length(s.nodes),length(freqs));I1=Matrix{ComplexF64}(undef,length(s.segments),length(freqs));I2=similar(I1)
    for (k,f) in pairs(freqs);V[:,k],I1[:,k],I2[:,k]=solve_frequency(s,2pi*f,ids,values);end
    (;frequencies=collect(freqs),voltage=V,i1=(I1-I2)/2,i2=I1+I2)
end

sample_time_axis(nyquist,n)=collect(0:n-1)/(2nyquist)
one_sided_frequency_axis(nyquist,n,fzero=1e-6)=[fzero;collect(1:n÷2)*(2nyquist/n)]
"Raised-cosine antialias response, flat then tapered to zero at Nyquist."
function tukey_antialias(nbins::Integer;alpha::Real=DEFAULT_TUKEY_ALPHA)
    0<=alpha<=1||throw(ArgumentError("Tukey alpha must be in [0,1]"));x=range(0,1,length=nbins);alpha==0&&return ones(nbins)
    [u<=1-alpha ? 1. : .5*(1+cos(pi*(u-(1-alpha))/alpha)) for u in x]
end
function double_exponential(t,imax,front;jones=false)
    vals=Dict("f1_2_5"=>(1.2e-6,1.25e6,2.8736e5),"f1_2_50"=>(1.2e-6,2.4691e6,1.4663e4),"f1_2_200"=>(1.2e-6,2.6247e6,3521.1),"f250_2500"=>(250e-6,9615.4,347.58));tf,a,b=vals[String(front)]
    k=(exp(-b*tf)-(jones ? exp(-(a*tf)^2) : exp(-a*tf)))/(a-b);[u>0 ? imax/(k*(a-b))*(exp(-b*u)-(jones ? exp(-(a*u)^2) : exp(-a*u))) : 0. for u in t]
end
function heidler(t,terms;imax=nothing)
    y=zeros(length(t));for q in terms;i0=Float64(q[:i0]);n=Float64(q[:n]);t1=Float64(q[:tau1]);t2=Float64(q[:tau2]);eta=exp(-(t1/t2)*(n*t2/t1)^(1/n));y .+=[u>0 ? i0/eta*(u/t1)^n/(1+(u/t1)^n)*exp(-u/t2) : 0 for u in t];end
    imax===nothing ? y : y/maximum(abs,y)*imax
end
function waveform(sig,t)
    String(sig[:waveform])=="doubleExp" ? double_exponential(t,Float64(sig[:imax]),String(sig[:front]);jones=Bool(getv(sig,:jones,false))) : haskey(sig,:terms) ? heidler(t,sig[:terms];imax=haskey(sig,:imax) ? Float64(sig[:imax]) : nothing) : error("legacy fixed-term Heidler requires terms")
end
"Transient solve with Tukey antialias low-pass (default alpha=0.75)."
function transient_response(s,sig;tukey_alpha=DEFAULT_TUKEY_ALPHA)
    n=Int(sig[:fftPoints]);ispow2(n)||error("fftPoints must be a power of two");nyq=Float64(sig[:nyquistHz]);t=sample_time_axis(nyq,n);current=waveform(sig,t)
    current.*=[.5*erfc((k-.8n)/(n/20)) for k=1:n];X=fft(current);freqs=one_sided_frequency_axis(nyq,n,Float64(getv(sig,:freqZeroHz,1e-6)))
    sw=run_sweep(s,freqs,[String(sig[:sourceNode])],[1+0im]);filt=tukey_antialias(length(freqs);alpha=tukey_alpha)
    function synth(H);half=H.*X[1:length(freqs)].*filt;half[1]=real(half[1]);half[end]=real(half[end]);real(ifft([half;conj.(reverse(half[2:end-1]))]));end
    nodes=String.(sig[:observeNodes]);volts=reduce(vcat,[permutedims(synth(sw.voltage[nodeindex(s,id),:])) for id in nodes]);elecs=String.(getv(sig,:observeElectrodes,String[]))
    i1=isempty(elecs) ? zeros(0,n) : reduce(vcat,[permutedims(synth(sw.i1[segmentindex(s,id),:])) for id in elecs]);i2=isempty(elecs) ? zeros(0,n) : reduce(vcat,[permutedims(synth(sw.i2[segmentindex(s,id),:])) for id in elecs])
    (;time=t,injected_current=current,node_ids=nodes,voltage=volts,electrode_ids=elecs,i1=i1,i2=i2,tukey_alpha=Float64(tukey_alpha))
end
function write_csv(path,r)
    open(path,"w") do io
        print(io,"time_s,injected_current_A");for id in r.node_ids;print(io,",voltage_",id,"_V");end;for id in r.electrode_ids;print(io,",i1_",id,"_A,i2_",id,"_A");end;println(io)
        for k=eachindex(r.time);@printf(io,"%.16e,%.16e",r.time[k],r.injected_current[k]);for i=eachindex(r.node_ids);@printf(io,",%.16e",r.voltage[i,k]);end;for i=eachindex(r.electrode_ids);@printf(io,",%.16e,%.16e",r.i1[i,k],r.i2[i,k]);end;println(io);end
    end
end
"Save injected current and node-voltage waveforms from a transient result."
function write_transient_plot(path::AbstractString,r)
    time_us=r.time .* 1e6
    current_plot=plot(time_us,r.injected_current ./ 1e3;
        xlabel="Time (μs)",ylabel="Current (kA)",label="Injected current",
        linewidth=2,grid=true,legend=:topright)
    voltage_plot=plot(;xlabel="Time (μs)",ylabel="Voltage (kV)",
        grid=true,legend=:topright)
    for (i,id) in pairs(r.node_ids)
        plot!(voltage_plot,time_us,r.voltage[i,:] ./ 1e3;
            label="Voltage $id",linewidth=2)
    end
    figure=plot(current_plot,voltage_plot;layout=(2,1),size=(900,700),
        plot_title="TUPÃ transient response")
    savefig(figure,path)
    path
end
function run_file(path::AbstractString)
    s,root=load_study(path);base=splitext(basename(path))[1]
    if haskey(root,:signal);r=transient_response(s,root[:signal]);out=base*"_transient_results.csv";figure=base*"_transient_plot.png";write_csv(out,r);write_transient_plot(figure,r);println("wrote $out and $figure (Tukey antialias alpha=$(r.tukey_alpha))");return r
    elseif haskey(root,:sources)&&haskey(root,:frequencies);f=root[:frequencies];count=round(Int,Float64(f[:pointsPerDecade])*log10(Float64(f[:max])/Float64(f[:min])))+1;freqs=10 .^ range(log10(Float64(f[:min])),log10(Float64(f[:max])),length=count);ids=String[x[:node] for x in root[:sources]];vals=ComplexF64[complex(Float64(x[:current][:re]),Float64(x[:current][:im])) for x in root[:sources]];return run_sweep(s,freqs,ids,vals)
    end
    println("$(s.title): $(length(s.nodes)) nodes, $(length(s.segments)) electrode segments");s
end
end

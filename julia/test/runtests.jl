using Test, Tupa, FFTW

@testset "Tukey antialias filter" begin
    w=tukey_antialias(9;start=.85)
    @test w[1] == 1
    @test w[3] == 1
    @test w[end] == 0
    @test all(diff(w) .<= 0)
    @test all(tukey_antialias(9) .== 1)
    @test tukey_antialias(101;start=.85)[[93,94]] |> sum ≈ 1
    @test_throws ArgumentError tukey_antialias(8;start=1.1)
    @test_throws ArgumentError tukey_antialias(8;start=0)
end

@testset "axes and waveforms" begin
    @test sample_time_axis(1000,8)[2] == 1/2000
    @test one_sided_frequency_axis(1000,8,1e-6) == [1e-6,250,500,750,1000]
    i=double_exponential(sample_time_axis(1e6,1024),30e3,"f1_2_50")
    @test all(isfinite,i)
    @test 29e3 < maximum(i) < 32e3
end

@testset "common JSON structure" begin
    s,_=load_study(joinpath(@__DIR__,"..","..","common","buried_conductor_short.json"))
    @test length(s.segments)==2
    @test length(s.nodes)==3
    prepare!(s)
    v,_,_=solve_frequency(s,2pi*1000,["Node_1"],[1+0im])
    @test all(isfinite,v)
    # Unit injection at Node_1 enters the only segment attached to it: end current I1 = 1 A.
    sw=run_sweep(s,[1e3],["Node_1"],[1+0im])
    @test sw.i1[1,1] ≈ 1
end

@testset "transient antialias is opt-in" begin
    s,root=load_study(joinpath(@__DIR__,"..","..","common","portela1997_transient.json"))
    base=transient_response(s,root[:signal])
    same=transient_response(s,root[:signal];antialias_start=1.0)
    @test base.voltage == same.voltage
    filtered=transient_response(s,root[:signal];antialias_start=.85)
    @test all(isfinite,filtered.voltage)
    @test maximum(abs,filtered.voltage[1,:]) < maximum(abs,base.voltage[1,:])
end

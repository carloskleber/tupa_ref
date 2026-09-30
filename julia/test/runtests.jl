using Test, Tupa, FFTW

@testset "Tukey antialias filter" begin
    w=tukey_antialias(9;alpha=.15)
    @test w[1] == 1
    @test w[3] == 1
    @test w[end] == 0
    @test all(diff(w) .<= 0)
    @test_throws ArgumentError tukey_antialias(8;alpha=1.1)
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
end

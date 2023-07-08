# Requires the tutorial `euclidean_transformations.jl`.

using Makie
using Makie.Colors
using GLMakie

a = (2.0, 0.0, 0.0)
b = (-2.0, 1.0, 0.0)
Π = @r3 a::1 ∧ b::1

rotate_3d(x, Π, α) = rotate_3d(x, @r3 exp(-α::0 / 2::0 ⟑ unitize(Π::2)))
rotate_3d(x, Ω) = @r3 Tuple x::1 << Ω::(0, 2)

function rotating_plot()
  scene = Scene(backgroundcolor = :black, fxaa = true, resolution = (1920, 1080))
  cam3d!(scene; projectiontype = Makie.Perspective)
  update_cam!(scene, Vec3f(4.0, 4.0, 2.0), Vec3f(0, 0, 0))
  cam = cameracontrols(scene)
  lines!(scene, [(0.0, 0.0, 0.0), @ga 3 Tuple dual(Π::2)]; color = "#bbaaff")
  arrows!(scene, [Point3f(0.0, 0.0, 0.0)], Vec3f.([a, b]); color = [:lightgreen, :cyan], linewidth = 0.05, arrowsize = 0.15)
  poly!(scene, Point3f[(0.0, 0.0, 0.0), a, a .+ b, b], color = "#bbaaff44")

  start = (0.0, -1.0, 0.0)
  prev = start
  record(scene, joinpath(@__DIR__, "color_animation.mp4"), 0:2:360; framerate = 60) do angle
    if !isempty(scene.plots)
      for plot in scene.plots
        isa(plot, Mesh) && delete!(scene, plot)
      end
    end
    location = rotate_3d(start, Π, deg2rad(angle))
    mesh!(scene, Sphere(Point3f(location), 0.1), color="#33bbff")
    lines!(scene, Point3f[prev, location], color=:cyan)
    prev = location
    scene
  end
end

rotating_plot()

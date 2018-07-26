require 'rake/clean'


EXE01 = "Collatz"
EXE02 = "CollatzPar"
EXE03 = "CollatzParMap"
CLOBBER.include(EXE01, EXE02, EXE03)
CLEAN.include("*.o","*.hi")

def run(p)
  sh "./#{p} 300000 +RTS -N -s"
end
def compile(p)
  sh "ghc -O2 -threaded -rtsopts #{p}.hs"
end

desc "run"
task :run => [EXE01,EXE02,EXE03] do
  run(EXE01)
  run(EXE02)
  run(EXE03)
end

file EXE01 => "#{EXE01}.hs" do
  compile(EXE01)
end
file EXE02 => "#{EXE02}.hs" do
  compile(EXE02)
end
file EXE03 => "#{EXE03}.hs" do
  compile(EXE03)
end

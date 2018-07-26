require 'rake/clean'

Bin="obfus"
CLEAN.include("*.hi","*.o")
CLOBBER.include(Bin)

file Bin => FileList.new("*.hs") do
  sh "ghc --make rotate.hs -o #{Bin}"
end

task :run => Bin do
  sh "./#{Bin} test.txt"
end

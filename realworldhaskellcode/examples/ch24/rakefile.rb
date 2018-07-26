require 'rake/clean'
require 'benchmark'

Program="ch24test"
Output="output.out"

MainHs="LineCount.hs"
TestDir="Test"
Executable = "dist/build/#{Program}/#{Program}"
CLEAN.include(Output,"**/*.o","**/*.hi","dist","#{Program}.zip")
SrcFiles = FileList.new('*.hs')

file Executable => SrcFiles do
  sh "ghc -O2 -o #{Program} -outputdir tmp --make #{MainHs}"
end
task :build => [Executable]
task :parallel do
  sh "ghc -O2 -o #{Program}_threaded -outputdir tmp --make -threaded #{MainHs} -fforce-recomp +RTS -N2 "
end

task :default => [:clean, :build]


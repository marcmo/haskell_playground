require 'rake/clean'

MainHs="Main.hs"
Input="NvmConfigBDC.pgm"

TestDir="Test"
Executable = "nvram"
Workspace="/home/omueller/dev/git/basisSW.git/"

CLEAN.include("**/*.o","**/*.hi","dist","#{Executable}.zip","*.prof")

SrcFiles = FileList.new('*.hs')
file Executable => SrcFiles do
  sh "ghc -O2 -o #{Executable} -outputdir tmp --make #{MainHs}"
end

desc "run compatibility tests on #{Workspace}"
file :run => Executable do
	puts %x[./#{Executable} #{Workspace}]
end

task :build => [Executable]

task :default => [:clean, :build]

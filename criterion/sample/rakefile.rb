require 'rake/clean'
require 'benchmark'

Program="Fibber"
Executable = "#{Program}"
Executable2 ="factorial"

MainHs="Fibber.hs"

CLEAN.include("**/*.o","**/*.csv","**/*.hi","tmp","*.exe",Executable,Executable2,"*.png","#{Executable}.html")
SrcFiles = FileList.new('*.hs')

file Executable => SrcFiles do
  buildHs(Program,MainHs)
end

desc "run program on data"
file :run => Executable do
	sh "time ./#{Executable} -o #{Executable}.html"
end
task :build => [Executable]

desc "execute factorial benchmark with barchart output"
task :factorial do
  buildHs("factorial","factorial.hs")
  createChart("factorial")
end

def createChart(prog)
  sh "./#{prog} --summary=factorial.csv"
  sh "barchart criterion #{prog}.csv"
  sh "xzgv #{prog}.png &"
end

def buildHs(prog,main)
  sh "ghc -O2 -o #{prog} -outputdir tmp --make #{main} -fforce-recomp"
end

task :default => [:clean, :build]

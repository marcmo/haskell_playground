require 'rake/clean'
require 'benchmark'

Programs=
  {"Minfree"=>"pearl_01_smallest_free_number.hs","Surpassing"=>"pearl_02_surpassing.hs"}


CLEAN.include("**/*.o","**/*.csv","**/*.hi","tmp","*.exe","*.png") << Programs.keys
task :removeExecutables do
  rm Dir.entries(".").select { |x| File.file?(x) && File.executable?(x) }
end
task :clean => :removeExecutables
SrcFiles = FileList.new('*.hs')

def createChart(prog)
  sh "./#{prog} --summary=#{prog}.csv"
  sh "barchart criterion #{prog}.csv"
  sh "xzgv #{prog}.png &"
end

def buildHs(prog,main)
  sh "ghc -O2 -o #{prog} -outputdir tmp --make #{main} -fforce-recomp"
end

def createTask(exe,main)
  namespace exe do
    file exe => SrcFiles do
      buildHs(exe,main)
    end

    desc "run some benchmarks for #{main}"
    task :run => exe do
      # sh "./#{exe} -t win -k win"
      sh "./#{exe} --summary=#{exe}.csv"
    end
    desc "create png benchmarks"
    task :png => exe do
      sh "./#{exe} -t png:450x175 -k png:450x175"
      sh "mv *.png tmp"
    end

    task :build => [exe]

    desc "execute benchmark with barchart output"
    task :chart => :exe do
      createChart("#{exe}_chart")
    end

  end
end

# task :default => [:clean, :build]
Programs.each {|k,v| createTask(k,v) }


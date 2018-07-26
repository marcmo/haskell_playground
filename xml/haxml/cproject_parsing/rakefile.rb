require 'rake/clean'
require 'benchmark'

Executable="parser"
MainHs="main.hs"
Input="cproject.xml"
module OS
  def OS.windows?
    (RUBY_PLATFORM =~ /cygwin|mswin|mingw|bccwin|wince|emx/) != nil
  end
  def OS.mac?
    (RUBY_PLATFORM =~ /darwin/) != nil
  end
  def OS.unix?
    !OS.windows?
  end
  def OS.linux?
    OS.unix? and not OS.mac?
  end
end
if OS.mac?
  ProjectPath="/Volumes/macbox_cs/dev/git/femclone.git"
elsif OS.linux?
  ProjectPath="/home/omueller/dev/git/FEM"
end

CLEAN.include("*.o","*.hi",Executable,"tmp","dist","output.*","output_reduced.*","*.prof","*.aux","*.hp","*.ps")
SrcFiles = FileList.new("**/*.hs")

file Executable => SrcFiles do
  sh "ghc -O2 -o #{Executable} -outputdir tmp --make #{MainHs}"
end

task :run => Executable do
  sh "./#{Executable} 1 #{ProjectPath}"
end

def generateDependencies(option)
  benchmark = Benchmark.realtime do
    sh "./#{Executable} #{option} #{ProjectPath}"
  end
  puts "generating dependencies step took: #{sprintf('%.2f', benchmark)} s"
end

def dot2pdf(name)
  sh "dot -Tpdf #{name}.dot -o #{name}.pdf"
end


desc 'run dot to produce graph'
task :runDot => Executable do
  generateDependencies 4
  benchmark = Benchmark.realtime do
    dot2pdf("output")
    sh "tred < output.dot > output_reduced.dot"
    dot2pdf("output_reduced")
    # sh 'gs -dBATCH -dNOPAUSE -sOutputFile=output.pdf -sDEVICE=pdfwrite  \-c "<< /PageSize [5000 2700]  >> setpagedevice"  -f output.ps'
    # rm "output.ps"
  end
  puts "generating dot output step took: #{sprintf('%.2f', benchmark)} s"
end

desc "generate dependency analysis"
task :dep => Executable do
  generateDependencies 2
end

desc "profiling"
task :prof => :clean do
  sh "ghc -o #{Executable} -outputdir tmp -O2 --make #{MainHs} -prof -auto-all -caf-all -fforce-recomp"
  benchmark = Benchmark.realtime do
    sh "time ./#{Executable} 2 #{ProjectPath} +RTS -p -K100M"
    # sh "time ./#{Executable} 2 #{ProjectPath} +RTS -hc -p -K100M"
    # sh "time ./#{Executable} 2 #{ProjectPath} +RTS -hy -p -K100M"
    # sh "time ./#{Executable} 2 #{ProjectPath} +RTS -hd -p -K100M"
    sh "hp2ps -e8in -c #{Executable}.hp"
  end
  puts "generating dependencies step took: " + sprintf("%.2f", benchmark)
end

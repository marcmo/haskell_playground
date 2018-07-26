require 'rake/clean'
require 'benchmark'
require 'highline/import'

Main="GraphGenerator"
MainHs="#{Main}.hs"
Output="path.out"
TmpFolder = "tmp"
#Profiling
ProfilingExecutable = "for_profiling"
CLEAN.include(TmpFolder,Output,"**/*.o","*.out","**/*.hi","dist","#{ProfilingExecutable}*")
SrcFiles = FileList['**/*.hs']

file ProfilingExecutable => SrcFiles do
  sh "ghc -O2 -o #{ProfilingExecutable} -outputdir #{TmpFolder} --make #{MainHs} -prof -rtsopts -auto-all -caf-all -fforce-recomp -main-is #{Main}"
end

namespace :prof do
  desc "time profiling"
  task(:t => [:clean,ProfilingExecutable]) { runProfiling("+RTS -p -K100M",false) }
  desc "heap profiling"
  task(:h => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hc -p -K100M",true) }
  desc "allocation-type profiling"
  task(:a => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hc -p -K100M",true) }
  desc "constructor-alloc-type profiling"
  task(:c => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hd -p -K100M",true) }
end

def runProfiling(options,psOutput)
  benchmark = Benchmark.realtime do
    sh "time ./#{ProfilingExecutable} #{options}"      
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  if psOutput
    sh "hp2ps -e8in -c #{ProfilingExecutable}.hp"
  end
end

task :default => [:clean, :build]

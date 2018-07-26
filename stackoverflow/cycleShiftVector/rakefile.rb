require 'rake/clean'
require 'benchmark'

MainHs='cycleShiftVector_7003572.hs'
MainHs2='cycleShiftVector_myVersion.hs'
Exe="cycleShift"
ProfilingExecutable = "cycleShift_profiling"
Exe2="cycleShift2"
SrcFiles=FileList["**/*.hs"]
Out="bin"
directory Out
CLEAN.include("**/*.hi","**/*.o",Out)

def create_tasks(exe,main)
  file exe => [Out] + SrcFiles do
    sh "ghc -O2 -o #{Out}/#{exe} -outputdir #{Out} --make #{main} -fforce-recomp"
    puts 'compiled!'
  end
  desc "build and run #{exe}"
  task "run_#{exe}" => exe do
    benchmark = Benchmark.realtime do
      puts `./#{Out}/#{exe}`
    end
    puts "running the programm took: " + sprintf("%.2f", benchmark) + " second(s)"
  end
end

file ProfilingExecutable => [Out] + SrcFiles do
  sh "ghc -O2 -o #{Out}/#{ProfilingExecutable} -outputdir #{Out} --make #{MainHs2} -prof -auto-all -caf-all -fforce-recomp -rtsopts"
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
    sh "time ./#{Out}/#{ProfilingExecutable} #{options}"      
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  if psOutput
    sh "hp2ps -e8in -c #{ProfilingExecutable}.hp"
  end
end

create_tasks(Exe,MainHs)
create_tasks(Exe2,MainHs2)

task :default => :run


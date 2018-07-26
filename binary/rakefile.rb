require 'rake/clean'
require 'benchmark'

Out="bin"
ParseBin="#{Out}/parseBin"
CLEAN.include('**/*.hi','**/*.o')
CLOBBER.include(Out)
ProfilingExecutable = "#{Out}/for_profiling"
MainHs="parsing.hs"
SrcFiles=FileList.new("**/*.hs")
Input="testRam.bin"
Output="#{Out}/out.json"

directory Out

desc 'build binary parser'
file ParseBin => [Out,SrcFiles] do
  sh "ghc -O2 --make #{MainHs} -outputdir #{Out} -o #{ParseBin}"
end

desc 'run on input'
task :run, :num do |t,args|
  sh "time ./#{ParseBin} #{Input} #{Output} #{args.num}"
end
task :run => ParseBin

task :default => [:run]

file ProfilingExecutable => [Out,SrcFiles] do
  sh "ghc -O2 --make #{MainHs} -outputdir #{Out} -o #{ProfilingExecutable} -prof -auto-all -caf-all -fforce-recomp"
end

def createProfilingTask(t,description,arguments,psOut)
  namespace :prof do
    desc description
    # task(t => [:clean,ProfilingExecutable]) { runProfiling(arguments,psOut) }
    task t, :num do |t,args|
      runProfiling(arguments,psOut,args.num)
    end
    task t => [:clean,ProfilingExecutable]
  end
end

{ :time => ["time profiling","+RTS -p -K100M",false],
  :heap => ["heap profiling","+RTS -hc -p -K100M",true],
  :alloc => ["allocation-type profiling","+RTS -hc -p -K100M",true],
  :constructor => ["constructor-alloc-type profiling","+RTS -hd -p -K100M",true]
}.each {|k,v| createProfilingTask(k,v[0],v[1],v[2])}

def runProfiling(options,psOutput,num)
  benchmark = Benchmark.realtime do
    sh "time ./#{ProfilingExecutable} #{Input} #{Output} #{num} #{options}"      
    mv Dir.glob("for_profiling*"),Out
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  cd Out do
    if psOutput
      sh "hp2ps -e8in -c for_profiling.hp"
    end
  end
end


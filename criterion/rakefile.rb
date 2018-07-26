require 'rake/clean'
require 'benchmark'

EXE='folds'
CLEAN.include("**/*.hi","**/*.o",EXE)

file EXE => FileList.new("**/*.hs") do
  puts `ghc -O --make folds.hs`
  puts 'compiled!'
end
desc 'build and run'
task :run => EXE do
  benchmark = Benchmark.realtime do
    puts `./folds`
  end
  puts "running the programm took: " + sprintf("%.2f", benchmark) + " second(s)"
end

task :default => :run


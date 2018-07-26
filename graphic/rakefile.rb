require 'rake/clean'
CLEAN.include(["*.o","*.hi"])
CLOBBER.include(["*.o","*.hi","sample","myTests"])
task :default do
  sh "ghc chartSample.hs -o sample --make"
  sh "./sample"
end

desc 'generate chart examples'
task :chart do
  sh "runhaskell all_tests.hs --png"
end

desc 'generate a view special chart examples'
task :chart2 do
  sh "ghc all_tests.hs -o myTests --make"
  sh "./myTests --test"
end


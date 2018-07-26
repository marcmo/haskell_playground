require 'rake/clean'


outputfile = "output.html"
CLEAN = FileList[outputfile]

task :configure do
  sh "runhaskell Setup.lhs configure"
end
task :build do
  sh "runhaskell Setup.lhs build"
end

task :default => [:clean, :configure, :build]


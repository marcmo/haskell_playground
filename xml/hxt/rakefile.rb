require 'rake/clean'

Simple='simple'
CLEAN.include('*.o','*.hi' ,Simple)

task Simple => FileList['simple.hs'] do
  sh "ghc simple.hs"
end

desc 'run simple example'
task :run => Simple do
  sh "./#{Simple} simple.xml"
end


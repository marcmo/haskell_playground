require 'rake/clean'

Executable="iching"
MainHs="#{Executable}.hs"
Input="#{Executable}.xml"

CLEAN.include("*.o","*.hi",Executable)
task :default => :build

task :build do
  sh "ghc --make #{MainHs} -XNoMonomorphismRestriction"
end

task :run do
  sh "./#{Executable} #{Input}"
end


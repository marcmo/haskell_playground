require 'rake/clean'
require 'benchmark'

Output="output.out"
if (RUBY_PLATFORM =~ /darwin/) != nil
  Out="/Volumes/macbox_cs/oliver/tmp/buildoutput/sparklines"
else
  Out="/home/omueller/tmp/buildoutput/sparklines"
end

SrcFiles = FileList.new('**/*.hs')
Tmp="tmp"
Executable="femsparks"
MainHs="Main.hs"
CLEAN.include("**/*.o","**/*.hi",Executable,Tmp,"*.png",Out)

directory Out

file Executable => [Out] + SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{Out}/#{Executable} -outputdir #{Out} --make #{MainHs} -threaded -fforce-recomp"
  stripExec Executable
end
task :build => [Executable] do
  FileUtils.rm_rf Tmp, :verbose => true
end

desc "create sparklines for fem"
task :fem => Executable do
  sh "#{Out}/#{Executable}"
  FileUtils.mv FileList.new("*.png"), Out, :verbose => true
end

task :default => :build

def stripExec (x)
  # if OS.unix?
    # sh "strip #{Out}/#{Executable}"
    # sh "upx #{Out}/#{Executable}"
  # end
end

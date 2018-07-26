require 'rake/clean'
require 'benchmark'

CLEAN.include("**/*.o","**/*.hi")
desc "list exes"
task :listExecutalbes do
  puts Dir.glob("**/*").select { |x| File.file?(x) && File.executable?(x) }
end
task :removeExecutables do
  rm Dir.glob("**/*").select { |x| File.file?(x) && File.executable?(x) }
end
task :clean => :removeExecutables
SrcFiles = FileList.new('*.hs')


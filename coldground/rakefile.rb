require 'rake/clean'

WorkingDir = File.expand_path('.')
Distdir = WorkingDir + '/dist'
distdir2 = 'c:/Downloads/personal/dropbox/code/haskell/mycode/coldground/dist/build/gen'
Imagedir = '/Users/oliver/Programming/webdev/slimbox/sections'
Imagedir2 = 'c:/Downloads/personal/pics'
TmpWebDev = '/Users/oliver/webdev'
Generator = TmpWebDev + '/gen'
Outputfile = TmpWebDev + '/output.html'
Result = TmpWebDev + '/' + Outputfile
SrcFiles = FileList.new(WorkingDir + '/src/**/*.*')
CLEAN.include(Outputfile)
CLEAN.include(TmpWebDev + '/*')
CLEAN.include(Distdir)

task :gen => FileList.new(Generator,Outputfile) + SrcFiles

directory TmpWebDev
task :dist => TmpWebDev do
  puts Distdir
  rm_r Dir.glob(TmpWebDev + '/*')
  mv Dir.glob(Distdir + '/build/gen/*'), TmpWebDev, :verbose => false
end

file Generator => SrcFiles do
  puts '...building again...'
  runhaskell 'configure'
  runhaskell 'build'
  Rake::Task[:dist].invoke
end

file Outputfile => SrcFiles do
  generate
end

def generate
  sh Generator + ' ' + Imagedir + ' ' + Outputfile
end
  
task :gen2 do
  sh distdir2+'/gen.exe '+Imagedir2+' '+Outputfile
end

task :default => [:clean, :build, :gen]

def runhaskell arg
  sh 'runhaskell Setup.lhs ' + arg
end

class Task 
  def investigation
    result = '------------------------------\n'
    result << 'Investigating #{name}\n' 
    result << 'class: #{self.class}\n'
    result <<  'task needed: #{needed?}\n'
    result <<  'timestamp: #{timestamp}\n'
    result << 'pre-requisites: \n'
    prereqs = @prerequisites.collect {|name| Task[name]}
    prereqs.sort! {|a,b| a.timestamp <=> b.timestamp}
    prereqs.each do |p|
      result << '--#{p.name} (#{p.timestamp})\n'
    end
    latest_prereq = @prerequisites.collect{|n| Task[n].timestamp}.max
    result <<  'latest-prerequisite time: #{latest_prereq}\n'
    result << '................................\n\n'
    return result
  end
end

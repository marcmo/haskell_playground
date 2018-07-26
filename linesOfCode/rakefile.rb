require 'rake/clean'

outputfile = "output.html"
CLEAN = FileList[outputfile]

task :configure do
  sh "runhaskell Setup.lhs configure"
end
task :build do
  sh "runhaskell Setup.lhs build"
end
task :test2 do
	sh "mv dist\\build\\cppunittestcount\\cppunittestcount.exe c:\\tmp\\unittestcounting"
	copyTask 'dist/build/cppunittestcount/*.exe', 'articles'
end

task :default => [:clean, :configure, :build, :test2]

def copyTask srcGlob, targetDirSuffix, taskSymbol
  targetDir = File.join BUILD_DIR, targetDirSuffix
	p targetDir
  mkdir_p targetDir, QUIET
  FileList[srcGlob].each do |f|
    target = File.join targetDir, File.basename(f)
    file target => [f] do |t|
      cp f, target
    end
  end
end


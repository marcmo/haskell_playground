require 'rake/clean'

CLEAN.include("**/*.o","**/*.hi","dist")
Executable = 'dist/build/project_creator/project_creator'
SrcFiles = FileList.new('*.hs')
file Executable => SrcFiles do
  sh "runhaskell Setup.lhs configure"
  sh "runhaskell Setup.lhs build"
end

task :install => Executable do
  sh "sudo runhaskell Setup.lhs install"
end

task :build => [Executable]

task :default => [:clean, :build]



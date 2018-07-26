
class Problem
  attr_reader :initials

  def initialize(initials)
    @initials = initials
  end
end
def remove_choice(xs)
  x = xs[-1]
  xs.slice!(xs.size-1)
  return x
end
def treesearch(problem)
  frontier = problem.initials
  while(true)
    puts "true"
    break if frontier.length == 0
    frontier = []
    s = remove_choice(frontier)
    return if s == problem.goal
  end
end

p = Problem.new([1])
treesearch(p)

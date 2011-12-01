require 'optparse'
require 'cobplexity'

analyzer = Cobplexity::Analyzer.new

OptionParser.new do |opts|
  opts.banner = "Usage: cobplexity [options]"
  opts.on("-g", "--group NAME", String, "Top level categroy for select code, defaults to 'APPLICATION'") do |group|
    analyzer.group = group
  end
  opts.on("-t", "--threshold NUM", String, "Sets the threshold for code complexity. Complexity greater than threshold will be negative. If not specified, complexity is always a positive number") do |threshold|
    analyzer.threshold = threshold.to_i
  end
  opts.on("-f", "--files a,b,c", Array, "Required: List of files to analyze, accepts wildcards") do |files|
    analyzer.files.concat files
  end
end.parse!

analyzer.analyze.each do |metric|
  puts metric.to_s
end

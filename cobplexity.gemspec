Gem::Specification.new do |s|

	s.name = "cobplexity"
	s.version = '0.1.0'
	s.platform = Gem::Platform::RUBY
	s.authors = ['Guy Royse']
	s.email = ['guy@guyroyse.com']
	s.homepage = "http://github.com/guyroyse/cobplexity"
	s.license = "http://creativecommons.org/licenses/by-sa/3.0/"
	s.summary = "Generates CSV files of static code analysis for COBOL modules"
	s.description = "Generates CSV files that can be consumed by Microsoft Treemapper from COBOL source modules"

	s.required_rubygems_version = ">=1.3.6"

	s.files = Dir["lib/**/*"] 
  s.files += Dir["bin/*"]
	
  s.executables << 'cobplexity'

	s.require_path = 'lib'

end

class Module
  attr_accessor :code
  def lines
    @code.lines.count do |line|
      !line.strip.empty?
    end
  end
end

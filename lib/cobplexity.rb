class Module
  attr_accessor :code
  def lines
    @code.lines.count do |line|
      !line_blank? line
    end
  end
end

def line_blank? line
  line.strip.empty? || (line.strip.length < 7) 
end

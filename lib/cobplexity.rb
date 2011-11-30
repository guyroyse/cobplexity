class Module
  attr_reader :code, :lines
  def code= code
    @code = code
    @lines = @code.lines.count do |line|
      cobol_line = Line.new line
      !cobol_line.blank? && !cobol_line.comment?
    end
  end
end

class Line
  def initialize line
    @line = line
  end
  def blank?
    @line.strip.empty? || (@line.strip.length < 7) 
  end
  def comment?
    comment_char = ''
    comment_char = @line[6] unless self.blank?
    !comment_char.strip.empty?
  end
end


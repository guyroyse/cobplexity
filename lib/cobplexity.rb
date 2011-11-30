module Cobplexity

  class Module
    attr_reader :code, :lines
    def code= code
      @code = code
      @lines = 0
      @code.lines.each do |line|
        cobol_line = Line.new line
        @lines = 0 if cobol_line.procedure_division_header?
        @lines+= 1 if cobol_line.executable?
      end
    end
  end

  class Line
    def initialize line
      @line = line
    end
    def executable?
      !self.blank? && !self.comment? && !self.continuation? && !self.procedure_division_header?
    end
    def blank?
      @line.strip.empty? || (@line.strip.length < 7) 
    end
    def comment?
      self.column_7 == '*'
    end
    def continuation?
      self.column_7 == '-'
    end
    def column_7
      self.blank? ? ' ' : @line[6]
    end 
    def procedure_division_header?
      @line.match /PROCEDURE DIVISION/
    end
  end

end

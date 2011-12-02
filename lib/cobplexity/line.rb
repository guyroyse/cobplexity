module Cobplexity

  class Line
    def initialize line
      @line = line.rstrip
    end
    def code?
      !self.blank? && !self.comment? && !self.continuation? && !self.paragraph? && !self.procedure_division?
    end
    def blank?
      self.statement.empty?
    end
    def paragraph?
      !self.comment? && !self.continuation? && !self.area_a.empty? && !self.copy_statement?
    end
    def comment?
      self.control == '*'
    end
    def continuation?
      self.control == '-'
    end
    def branches
      self.statement.split.count do |item|
        ['IF', 'ELSE', 'WHEN', 'WHILE', 'UNTIL', 'AND', 'OR'].include? item.upcase
      end
    end
    def paragraph_name
      self.statement.strip.delete '.'
    end
    def copy_statement?
      self.statement.match /COPY /i
    end
    def procedure_division?
      self.statement.match /PROCEDURE DIVISION\./i
    end
    def area_a
      self.statement[0..3].strip
    end
    def control
      @line.length > 6 ? @line[6] : ' '
    end
    def statement
      @line.length > 7 ? @line[7..71] : ''
    end
  end

end

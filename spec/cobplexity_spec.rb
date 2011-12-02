require 'cobplexity'

describe Cobplexity::Module do

  it 'has code' do
    subject.code = 'foo'
    subject.code.should == 'foo'
  end

  context "when counting total lines of code" do 

    it 'counts lines of code' do
      subject.code = <<-eos
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
           CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end

    it "doesn't count blank lines" do
      subject.code = <<-eos
           MOVE 'Y' TO YES.

           CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "ignores line numbers when counting lines" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count comments" do
      subject.code = <<-eos
           MOVE 'Y' TO YES.
      *    CALL MOVE-IT.
           CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count continuations" do
      subject.code = <<-eos
           MOVE 'Y' TO YES.
           CALL
      -       MOVE-IT.
      eos
      subject.lines.should == 2
    end  

    it "counts lines with other characters in column 7" do
      subject.code = <<-eos
           MOVE 'Y' TO YES.
      /    CALL MOVE-IT.
           CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end  

    it "doesn't count the PROCEDURE DIVISION statement" do
      subject.code = <<-eos
       PROCEDURE DIVISION.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

    it "doesn't count lines before the PROCEDURE DIVISION" do
      subject.code = <<-eos
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

  end

  context "when there are paragraphs" do

    before :each do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      
       MOVE-IT.
           MOVE YES TO OUTPUT.
      
         MOVE-IT-AGAIN.
           MOVE YES TO OUTPUT.
      eos
    end

    it "doesn't count paragraph names in total lines of code" do
      subject.lines.should == 4
    end

    it "creates a paragraph entry for each paragraph" do
      subject.paragraphs.count.should == 3
    end

    it "ignores commented out paragraph names" do
      subject.code = <<-eos
      *OLD-MAINLINE.
       MAINLINE.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      eos
      subject.paragraphs.count.should == 1
    end

    it "adds a name to the paragraph entry" do
      subject.paragraphs[0].name.should == 'MAINLINE'
    end

    it "ignores whitespace when parsing paragraph name" do
      subject.paragraphs[2].name.should == 'MOVE-IT-AGAIN'
    end

    it "counts lines in a paragraph" do
      subject.paragraphs[0].lines.should == 2
    end

    it "doesn't count any paragraphs before the PROCEDURE DIVISION" do
      subject.code = <<-eos
       INVALID-PARAGRAPH.
          MOVE '1' TO INVALID-MEMORY.
      
       PROCEDURE DIVISION.
      
       MAINLINE.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      
       MOVE-IT.
           MOVE YES TO OUTPUT.
      
         MOVE-IT-AGAIN.
           MOVE YES TO OUTPUT.
      eos
      subject.paragraphs.count.should == 3
    end

    context "when there is a COPY in AREA A" do

      before :each do
        subject.code = <<-eos
         MAINLINE.
             MOVE 'Y' TO YES.
             CALL MOVE-IT.
         COPY AWESOME-SAUCE.
        eos
      end

      it "counts a COPY in AREA A as code" do
        subject.paragraphs[0].lines.should == 3
      end

      it "doesn't treat a COPY in AREA A as a paragraph" do
        subject.paragraphs.count.should == 1
      end

    end

  end

  context "when calculating complexity of a paragraph" do

    it "has a cyclomatic complexity of 1 for a simple paragraph" do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' TO YES.
           CALL MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 1
    end

    it "adds to the cyclomatic complextiy of there is an IF statement" do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' YES.
           IF YES THEN CALL MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 2
    end

    it "adds to the cyclomatic complextiy of there is more than one IF statement" do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' YES.
           IF YES THEN CALL MOVE-IT.
           IF NO THEN CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy of there is an ELSE statement" do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' YES.
           IF YES THEN
               CALL MOVE-IT
           ELSE
               CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy if there is an ELSE statement on the same line" do
      subject.code = <<-eos
       MAINLINE.
           MOVE 'Y' YES.
           IF YES THEN CALL MOVE-IT ELSE CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy if there is a WHEN statement" do
      subject.code = <<-eos
       MAINLINE.
           EVALUATE WS-FLAG
               WHEN 'Y'
                   CALL MOVE-IT
               WHEN 'N'
                   CALL DONT-MOVE-IT
           END-EVALUATE.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy if there is a WHILE statement" do
      subject.code = <<-eos
       MAINLINE.
           PERFORM MOVE-IT WHILE NOT END-OF-FILE.
      eos
      subject.paragraphs.last.complexity.should == 2
    end

    it "adds to the cyclomatic complextiy if there is an UNTIL statement" do
      subject.code = <<-eos
       MAINLINE.
           PERFORM MOVE-IT UNTIL END-OF-FILE.
      eos
      subject.paragraphs.last.complexity.should == 2
    end

  end

end

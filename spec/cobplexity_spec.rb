require 'cobplexity'

describe Cobplexity::Module do

  it 'has code' do
    subject.code = 'foo'
    subject.code.should == 'foo'
  end

  context "when counting total lines of code" do 

    it 'counts lines of code' do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010     CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end

    it "doesn't count blank lines" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.

100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count lines with only a line number" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count comments" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010*    CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count continuations" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010     CALL
100020-       MOVE-IT.
      eos
      subject.lines.should == 2
    end  

    it "counts lines with other characters in column 7" do
      subject.code = <<-eos
100000     MOVE 'Y' TO YES.
100010/    CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end  

    it "doesn't count the PROCEDURE DIVISION statement" do
      subject.code = <<-eos
100000 PROCEDURE DIVISION.
100010     MOVE 'Y' TO YES.
100020     CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

    it "doesn't count lines before the PROCEDURE DIVISION" do
      subject.code = <<-eos
100000 DATA DIVISION.
100010 WORKING-STORAGE SECTION.
100020 PROCEDURE DIVISION.
100030     MOVE 'Y' TO YES.
100040     CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

  end

  context "when there are paragraphs" do

    before :each do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' TO YES.
100070     CALL MOVE-IT.
100080
100090 MOVE-IT.
100100     MOVE YES TO OUTPUT.
100110
100120   MOVE-IT-AGAIN.
100130     MOVE YES TO OUTPUT.
      eos
    end

    it "doesn't count paragraph names in total lines of code" do
      subject.lines.should == 4
    end

    it "creates a paragraph entry for each paragraph" do
      subject.paragraphs.count.should == 3
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
100000 INVALID-PARAGRAPH.
100010    MOVE '1' TO INVALID-MEMORY.
100020
100030 PROCEDURE DIVISION.
100040
100050 MAINLINE.
100060     MOVE 'Y' TO YES.
100070     CALL MOVE-IT.
100080
100090 MOVE-IT.
100100     MOVE YES TO OUTPUT.
100110
100120   MOVE-IT-AGAIN.
100130     MOVE YES TO OUTPUT.
      eos
      subject.paragraphs.count.should == 3
    end

    it "counts a COPY in AREA A as code" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' TO YES.
100070     CALL MOVE-IT.
100080 COPY AWESOME-SAUCE.
      eos
      subject.paragraphs[0].lines.should == 3
    end

    it "doesn't treat a COPY in AREA A as a paragraph" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' TO YES.
100070     CALL MOVE-IT.
100080 COPY AWESOME-SAUCE.
      eos
      subject.paragraphs.count.should == 1
    end

  end

  context "when calculating complexity of a paragraph" do

    it "has a cyclomatic complexity of 1 for a simple paragraph" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' TO YES.
100070     CALL MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 1
    end

    it "adds to the cyclomatic complextiy of there is an IF statement" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' YES.
100070     IF YES THEN CALL MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 2
    end

    it "adds to the cyclomatic complextiy of there is more than one IF statement" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' YES.
100070     IF YES THEN CALL MOVE-IT.
100080     IF NO THEN CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy of there is an ELSE statement" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' YES.
100070     IF YES THEN
100080         CALL MOVE-IT
100090     ELSE
100100         CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

    it "adds to the cyclomatic complextiy of there is an ELSE statement on the same line" do
      subject.code = <<-eos
100050 MAINLINE.
100060     MOVE 'Y' YES.
100070     IF YES THEN CALL MOVE-IT ELSE CALL DONT-MOVE-IT.
      eos
      subject.paragraphs.last.complexity.should == 3
    end

  end

end

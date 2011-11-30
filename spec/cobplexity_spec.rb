require 'cobplexity'

describe Cobplexity::Module do

  it 'has code' do
    subject.code = 'foo'
    subject.code.should == 'foo'
  end

  context "when counting total lines of code" do 

    it 'counts lines of code' do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010     CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end

    it "doesn't count blank lines" do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.

100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count lines with only a line number" do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count comments" do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010*    CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 2
    end

    it "doesn't count continuations" do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010     CALL
100020-       MOVE-IT.
      eos
      subject.lines.should == 2
    end  

    it "counts lines with other characters in column 7" do
      subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010/    CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
      eos
      subject.lines.should == 3
    end  

    it "doesn't count the PROCEDURE DIVISION statement" do
      subject.code = <<-eos
100000 PROCEDURE DIVISION.
100010     MOVE 'Y' to YES.
100020     CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

    it "doesn't count lines before the PROCEDURE DIVISION" do
      subject.code = <<-eos
100000 DATA DIVISION.
100010 WORKING-STORAGE SECTION.
100020 PROCEDURE DIVISION.
100030     MOVE 'Y' to YES.
100040     CALL MOVE-IT.
      eos
      subject.lines.should == 2
    end

  end

  context "when there are paragraphs" do

    before :each do
      subject.code = <<-eos
100000 MAINLINE.
100010     MOVE 'Y' to YES.
100020     CALL MOVE-IT.
100030
100040 MOVE-IT.
100050     MOVE YES TO OUTPUT.
100060
100070 MOVE-IT-AGAIN.
100080     MOVE YES TO OUTPUT.
      eos
    end

    it "doesn't count paragraph names in total lines of code" do
      subject.lines.should == 4
    end

    it "creates a paragraph entry for each paragraph" do
      subject.paragraphs.count.should == 3
    end

  end

end

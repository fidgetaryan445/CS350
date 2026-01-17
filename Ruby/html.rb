class HTML
  def initialize
  end
  
  def tag(tagname)
    tagname = "p" if tagname[0..2] == "par"

    if block_given?
      printf "<%s>\n", tagname
      content = yield
	  print content
      printf "</%s>\n", tagname
    else
      printf "<%s>\n", tagname   
    end

    nil
  end

  alias method_missing tag

  def self.generate(out = STDOUT, &block)
    HTML.new.instance_eval(&block)
  end
end

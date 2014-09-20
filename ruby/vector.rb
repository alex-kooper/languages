class Vector
    attr_reader :values

    def initialize(values)
	@values = values
    end

    def +(other)
	self.class.new(@values.zip(other.values).map {|x, y| x + y})
    end

    def *(other)
	self.class.new(@values.map {|x| x * other})
    end

    def coerce(other)
	[self, other]	
    end

    def inspect
     	@values
    end

    alias_method :to_s, :inspect
end


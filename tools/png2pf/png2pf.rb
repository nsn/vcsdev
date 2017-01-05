require 'optparse'
require 'chunky_png'

def reverse(input, len)
    z=0
    len.times { |i| z=z*2+input[i] } 
    z
end

def bitstring(bits, len=0)
    rv = bits.to_s(2)
    rv = rv.rjust(len, "0")
end

# parse arguments

opts = {
    :rows => 192,
    :cols => 40,
    :blank => ChunkyPNG::Color.from_hex("#000000"),
    :debug => false,
    :prefix => "PFDATA"
}

OptionParser.new do |opt|
    opt.accept(ChunkyPNG::Color) do |c|
        ChunkyPNG::Color.from_hex(c)
    end
    opt.banner = "Usage: png2pf.rb [options] <inputFile>"
    opt.on("-r", "--rows=val", Integer, "number of rows to be considered, defaults to min(height of input image, #192)") { |o| opts[:rows] = o }
    opt.on("-c", "--cols=val", Integer, "number of columns to be considered, defaults to min(width of input image, #40)") { |o| opts[:cols] = o }
    opt.on("-b", "--blank-color=val", ChunkyPNG::Color, "color to consider as blank (#%0), all other colors are considered filled (#%1), format: #rrggbb. default = #000") { |o| opts[:blank] = o }
    opt.on("-d", "--debug", FalseClass, "outputs some debug to stderr") { |o| opts[:debug] = true }
end.parse!

inputFile = ARGV.shift
if inputFile.to_s.empty? 
    abort "input file  missing!"
end

img = ChunkyPNG::Image.from_file(inputFile)

width = [opts[:cols], img.width].min 
height = [opts[:rows], img.height].min 

blankColor = opts[:blank]

# process input image
PF0_MASK = 0xf000000000
PF1_MASK = 0x0ff0000000
PF2_MASK = 0x000ff00000
PF3_MASK = 0x00000f0000
PF4_MASK = 0x000000ff00
PF5_MASK = 0x00000000ff

strips = [ [], [], [], [], [], [] ]

for y in 0..height-1 do
    bits = 0x0
    for x in 0..width-1 do
        pixel = ChunkyPNG::Color::parse(img[x,y]) 
        bit = (pixel == blankColor) ? 0 : 1
        bits = (bits << 1)^bit
        #puts "#{x}x#{y}:  #{pixel} <-> #{blankColor} => #{bit}"
    end

    # now shift bits to be 40 bits wide 
    bits = bits << 40-width


    ## for PF register bit orders see http://www.randomterrain.com/atari-2600-memories-tutorial-andrew-davie-14.html
    # PF0 = 4 leftmost bits, reversed, stored into the high nibble (*phew*)
    pf0 = reverse((bits&PF0_MASK) >> 36, 4) << 4
    # PF1 = next 8 bits, stored in original order
    pf1 = (bits & PF1_MASK) >> 28
    # PF2 = next 8 bits, stored in reverse
    pf2 = reverse((bits & PF2_MASK) >> 20, 8)
    ## second half of playfield re-uses PF0 to PF2, same rules apply
    pf3 = reverse((bits&PF3_MASK) >> 16, 4) << 4
    pf4 = (bits & PF4_MASK) >> 8
    pf5 = reverse((bits & PF5_MASK), 8)
    
    strips[0].push(pf0)
    strips[1].push(pf1)
    strips[2].push(pf2)
    strips[3].push(pf3)
    strips[4].push(pf4)
    strips[5].push(pf5)

    warn "#{bitstring(bits, 40)} -> #{pf0.to_s(2).rjust(8, "0")} #{pf1.to_s(2).rjust(8, "0")} #{pf2.to_s(2).rjust(8, "0")} #{bitstring(pf3, 8)} #{bitstring(pf4, 8)} #{bitstring(pf5, 8)}" if opts[:debug]
end

# now output the strips
numStrips = (width+3).divmod(8)[0] + 1

puts opts[:prefix] 
puts

numStrips.times do |i|
    puts "#{opts[:prefix]}_#{i}"
    strips[i].each do |b|
        puts "    .byte #%#{bitstring(b, 8)}"
    end
    puts
end



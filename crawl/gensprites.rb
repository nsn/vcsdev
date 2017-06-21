require 'optparse'
require 'chunky_png'

opts = {
    :width => 1,
    :height => 8,
    :frames => 1,
    :scale => 1,
    :blank => ChunkyPNG::Color.from_hex("#000000"),
    :debug => false,
    :inverted => false,
    :prefix => "SPRITE"
}

OptionParser.new do |opt|
    opt.accept(ChunkyPNG::Color) do |c|
        ChunkyPNG::Color.from_hex(c)
    end
    opt.banner = "Usage: gensprites.rb [options] <inputFile>"
    opt.on("-w", "--width=val", Integer, "width of sprite in multiples of 8 (1=8px, 2=16px)") { |o| opts[:width] = o }
    opt.on("-h", "--height=val", Integer, "height of sprite in pixels") { |o| opts[:height] = o }
    opt.on("-f", "--frames=val", Integer, "number of frames (arranged along the y axis)") { |o| opts[:frames] = o }
    opt.on("-s", "--scale=val", Integer, "number of scanlines per input pixel") { |o| opts[:scale] = o }
    opt.on("-b", "--blank-color=val", ChunkyPNG::Color, "color to consider as blank (#%0), all other colors are considered filled (#%1), format: #rrggbb. default = #000") { |o| opts[:blank] = o }
    opt.on("-i", "--invert-output", FalseClass, "invert output - outputs the strips form the bottom up, useful if your line counter counts down instead of up") { |o| opts[:inverted] = true }
    opt.on("-p", "--prefixes=val", String, "label prefixes, comma separated") { |o| opts[:prefix] = o }
    opt.on("-d", "--debug", FalseClass, "outputs some debug to stderr") { |o| opts[:debug] = true }
end.parse!

inputFile = ARGV.shift
if inputFile.to_s.empty?
    abort "input file  missing!"
end


W = 8               # 8 pixels register width

img = ChunkyPNG::Image.from_file(inputFile)

sprites = opts[:prefix].split(/,/).map{ |p| p.strip }.select{ |p| p.to_s.empty? == FALSE }

# read input image
allSprites = {}
sprites.each { |sprite|
    currentSprite = []
    opts[:frames].times { |frame|
        currentFrame = []
        opts[:width].times { |w|
            currentPart = []
            opts[:height].times { |y|
                currentLine = 0
                W.times { |x|
                    pixel = img[x,y]
                    v = (ChunkyPNG::Color::a(pixel) != opts[:blank])?1:0
                    currentLine = (currentLine << 1)
                    currentLine = currentLine | v
                    #puts "%dx%d -> %d" % [x,y,v]
                }
                #puts "    .byte #%%%08b" % currentLine
                currentPart.push(currentLine)
            }
            currentFrame.push(currentPart)
        }
        currentSprite.push(currentFrame)
    }
    allSprites[sprite] = currentSprite
}


# output
allSprites.each { |name, frames|
    puts "%s:" % name
    frames.each_with_index { |frame, frameIndex|
        puts "%s_F%d:" % [name, frameIndex]
        frame.each_with_index { |part, partIndex|
            puts "%s_F%d_%d:" % [name, frameIndex, partIndex]
            if opts[:inverted] then 
                part.reverse!
            end
            part.each { |line|
                opts[:scale].times {
                    puts "    .byte #%%%08b" % line
                }
            }
        }
    }
}





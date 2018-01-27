require 'optparse'
require 'chunky_png'

opts = {
    :width => 1,
    :height => 8,
    :frames => 1,
    :scale => 1,
    :blank => 0,
    :debug => false,
    :inverted => false,
    :prefix => "SPRITE",
    :paddingtop => 0,
    :paddingbottom => 0
}

OptionParser.new do |opt|
    opt.accept(ChunkyPNG::Color) do |c|
        ChunkyPNG::Color.from_hex(c)
    end
    opt.banner = "generates sprite data to stdout, reads png input, only cosinders alpha channel"
    opt.banner = "Usage: gensprites.rb [options] <inputFile>"
    opt.on("-w", "--width=val", Integer, "width of sprite in multiples of 8 (1=8px, 2=16px)") { |o| opts[:width] = o }
    opt.on("-h", "--height=val", Integer, "height of sprite in pixels") { |o| opts[:height] = o }
    opt.on("-f", "--frames=val", Integer, "number of frames (arranged along the y axis)") { |o| opts[:frames] = o }
    opt.on("-s", "--scale=val", Integer, "number of scanlines per input pixel") { |o| opts[:scale] = o }
    opt.on("-b", "--blank=val", Integer, "alpha channel value to consider as blank (#%0), all other colors are considered filled (#%1), 0-255, default = 0") { |o| opts[:blank] = o }
    opt.on("-i", "--invert-output", FalseClass, "invert output - outputs the strips form the bottom up, useful if your line counter counts down instead of up") { |o| opts[:inverted] = true }
    opt.on("-p", "--prefixes=val", String, "label prefixes, comma separated") { |o| opts[:prefix] = o }
    opt.on("-t", "--padding-top=val", Integer, "frame top padding in pixels, not affected by scale") { |o| opts[:paddingtop] = o }
    opt.on("-b", "--padding-bottom=val", Integer, "frame bottom padding in pixels, not affected by scale") { |o| opts[:paddingbottom] = o }

    #opt.on("-d", "--debug", FalseClass, "outputs some debug to stderr") { |o| opts[:debug] = true }
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
                    imgy = y+opts[:height]*frame;
                    pixel = img[x,imgy]
                    v = (ChunkyPNG::Color::a(pixel) == opts[:blank])?0:1
                    currentLine = (currentLine << 1)
                    currentLine = currentLine | v
                }
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
    size = 0
    puts "%s:" % name
    frames.each_with_index { |frame, frameIndex|
        puts "%s_F%d:" % [name, frameIndex]
        frame.each_with_index { |part, partIndex|
            puts "%s_F%d_%d:" % [name, frameIndex, partIndex]
            opts[:paddingtop].times {
                size+=1
                puts "    .byte #%%%08b" % 0
            }
            if opts[:inverted] then 
                part.reverse!
            end
            part.each { |line|
                opts[:scale].times {
                    size+=1
                    puts "    .byte #%%%08b" % line
                }
            }
            opts[:paddingbottom].times {
                size+=1
                puts "    .byte #%%%08b" % 0
            }
        }
    }
    puts "%s_SIZE EQM $%s" % [name, size.to_s(16)]
}




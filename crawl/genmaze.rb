layer1 = %q(
########████████########████████
#  ###        ██#     ##███    █
##    ##█████ ██# ### ##███ █ ██
### ####█████ ██# ###       █ ██
#            ███#   # ##█ ███ ██
# ######███ ████### # ##█ █   ██
#    ###█       ##### ##█   ████
#### ###█ █ ████#     ##███ ████
████   █# # ####█ ██████### ####
████ █    #       ██████###   ##
████ ███###### #█████       # ##
██   ███### ##     ██ ██# ###  #
█  █████    ####██    ██# ######
█ ██████### ####████ ███#     ##
█    ███#   ####████ █    ### ##
█ ██  ██# # ####████ ███##### ##
# ###  #█ █   ██#### ###█████ ██
#   ##    ███    ###       ██ ██
#### ###█ ████ █########██    ██
#### ###█ ████    ######████ ███
#### #    ██ ███# ######█    ███
#### # #███  ███#         ██   █
#      #██  ████## #####██████ █
# ######██ ██      ##     ██   █
█ ██████## ## ##██ █  ██# ## ###
█   ████#  ## ##██   ███# ## ###
███ ██    #### #██████  # #    #
███    █#####  #██████ █###### #
███ ████#   # #   ███     #### #
█   ████# # #   █   █ ██#   #  #
█ █       #   ##███   ██###   ##
████████########████████########
).gsub("\n", "")

layer2 = %q(
########████████########████████
#          █████#      #█      █
### ####██   ███## ### #███ ████
###   ##████ █ █#            ███
#####    ██    █# #### #████  ██
####  ##███ ████# ###  #██████ █
#### ###█   ███   ##  ##███    █
#### ###█ █ ████# ## ###███ ████
████ ███# # ####█ ██ ███### ####
████     ######  ███ ███### ####
████ ███########███  ██       ##
██   █ █#######     ████##### ##
██ ███      ####███ ████#####  #
█  █   █### ####███  ███##### ##
█ ██ ███### ####████ ██       ##
█ ██  ██### ####████ ███##### ##
# ###  #█ █  ███#### ###█████ ██
#  ####  ██ ███  ###     ███  ██
##  ####███ ████#### ###████ ███
### ####███      ### ###██   ███
#         ███ ██#### ###██ █████
# ## ###█ ███ ██####         ███
# ##       ██ ██########████ ███
# ######██ ██ █ ## ## ##  ██ ███
█ ██████## ## ##██ ██ ██# ## ###
█ ██████##    ##██ █████#### ###
█         ## ###██       ### ###
███ ████# ## ###█████ ██####   #
███ ████#  # ##   ███      ### #
█    ███##      █ ███ ██## ### #
█ ██       #####█     ██#      #
████████########████████########
).gsub("\n", "")

layers = [layer1, layer2]


def reverse(n) 
    z = 0
    8.times { |i|
        z = z*2+n[i]
    }
    z
end

quadrants = [
]

# fill /w empty tiles
4.times { |x|
    quadrants[x] = []
    4.times { |y|
        quadrants[x][y] = []
        2.times { |z| 
            quadrants[x][y][z] = []
            8.times {
                quadrants[x][y][z].push(0)
            }
        }
    }
}

empty_chars = " "

# parse layers
2.times { |l|
    32.times { |y|
        32.times { |x|
            n = x % 8
            b = y % 8
            qx = x/8
            qy = y/8
            idx = x+y*32
            char = layers[l][idx];
            v = empty_chars.index(char) != nil ? 0 : 1
            r = quadrants[qx][qy][l][b]
            r = r | (v << n)
            quadrants[qx][qy][l][b] = r
            #puts "%dx%d -> %d = %s [%d/%d/%d/%d]: %d ==> %08b" % [x, y, idx, char, qx, qy, b, n, v, r]
        }
    }
}


# predefined quadrants
#quadrants[0][0][0] = [
#    0b11111111,
#    0b10101000,
#    0b10000011,
#    0b11110111,
#    0b11111111,
#    0b11111111,
#    0b11111111,
#    0b11111111
#]
#
#quadrants[3][3][1] = [
#    0b10101010,
#    0b10111001,
#    0b10000000,
#    0b11110111,
#    0b11111111,
#    0b11111111,
#    0b11111111,
#    0b01010101
#]

if ARGV[0] == "test" then
    zeds = []
    4.times { |x|
        zeds[x] = []
        4.times { |y|
            zeds[x][y] = rand(2)
        }
    }
    
    4.times { |y|
        8.times { |i|
            4.times { |x|
                print "|%08b" % [ reverse( quadrants[x][y][zeds[x][y]][i] ) ]
            }
            print "\n"
        }
        print "-------------------------------------\n"
    }
    exit
end

# output default

puts "MAZEDATA_0"
4.times { |y|
    4.times { |x|
        2.times { |z|
            puts "MAZE_%d_%d_%d" % [x , y, z]
            quadrants[x][y][z].each { |b|
                #puts "    .byte #%%%08b" % [reverse(b)]
                puts "    .byte #%%%08b" % [b]
            }
        }
    }
}
#quadrants.each_index { |x|
#    quadrants[x].each_index { |y|
#        quadrants[x][y].each_index { |z|
#            puts "MAZE_%d_%d_%d" % [x , y, z]
#            quadrants[x][y][z].each { |b|
#                #puts "    .byte #%%%08b" % [reverse(b)]
#                puts "    .byte #%%%08b" % [b]
#            }
#        }
#    }
#}

# output transposed

#quadrants.each_index { |x|
#    quadrants[x].each_index { |y|
#        quadrants[x][y].each_index { |z|
#            quadrant = quadrants[x][y][z]
#            puts "MAZE_%d_%d_%d" % [x , y, z]
#            8.times { |a|
#                bin = 0
#                7.downto(0) { |b|
#                    bin = bin << 1
#                    bin = bin | quadrant[b][a]
#                }
#                puts "    .byte #%%%08b" % [bin]
#            }
#        }
#    }
#}


raw = {
    '0'=> [ 
        0b0001,
        0b0011,
        0b0111,
        0b1111
    ],
    '1'=> [
        0b1000,
        0b1100,
        0b1110,
        0b1111
    ]
}

bytes = {
    'HI'=>4,
    'LO'=>0
}

hdirs = {
    '0'=>true,
    '1'=>false
}

count = 0
hdirs.each { |hdir, hflip| 
    raw.each { |vdir, arr|
        enum = hflip ? arr.each : arr.reverse_each
        puts "PF_%s_%s:" % [ hdir, vdir ]
        enum.each { |b|
            byte = (b << 4) | b
            4.times {
                puts "    .byte #%%%08b" % byte
                count += 1
            }
        }
    }
}

puts "PF_NONE:"
16.times {
    puts "    .byte #%00000000"
    count+=1
}

puts "PF_ALL:"
16.times {
    puts "    .byte #%11111111"
    count+=1
}
warn "generated %s bytes of ROM" % count


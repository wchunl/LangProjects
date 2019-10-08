#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
nl := Character nl.

"Code from charfreq.st"
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"Code from tree.st"
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ true ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postOrder: visitor [
      visitor value: char value: (0 bitAt: 1).
   ]

].


Leaf subclass: Tree [
   |left right|

   isLeaf [ ^ false ]
   getLeft [ ^ left ]
   getRight [ ^ right ]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   depthFirst: visitor prefix: string [
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]

   postOrder: visitor [
      left postOrder: visitor.
      right postOrder: visitor.
      visitor value: char value: (1 bitAt: 1).
   ]

].

"Code from outbits.st"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"Code fomr bistream.st"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"-------------- MAIN -------------"
"---- Code runs starting here ----"
"---------------------------------"
(Smalltalk arguments size < 2)
   ifTrue: [
      stderr << 'Too many or too little arguments provided!' << nl.
      ObjectMemory quit: 1.
   ].

|filein fileout|
d_opt := false.
t_opt := false.
c_opt := false.
u_opt := false.
hasFileIn := false.
hasFileOut := false.

"Code referenced from cmdline.st"
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
   (arg = nil) ifTrue: [
      opt = $d ifTrue: [d_opt := true]. 
      opt = $t ifTrue: [t_opt := true]. 
      opt = $c ifTrue: [c_opt := true]. 
      opt = $u ifTrue: [u_opt := true]. 
   ]
   ifFalse: [
      hasFileIn ifTrue: [
         hasFileOut := true.
         fileout := arg.
      ] ifFalse: [
         hasFileIn := true.
         filein := arg.
      ]
   ]
]
ifError: [
   stderr << 'Error: invalid option' << nl.
].


d_opt ifTrue: [
   stdout << '--Debug Mode Active--' << nl.
   stdout << 'filein: ' << filein << nl.
   stdout << 'fileout: ' << fileout << nl.
].

"---------------------------------"
"---------- COMPRESSION ----------"
"---------------------------------"
(t_opt | c_opt) ifTrue: [
   "Open file for streaming"
   stream := FileStream open: filein mode: FileStream read.

   "Create the frequence table and fill it"
   "Code referenced from catfile.st"
   freqTable := ZeroArray new: 257.
   freqTable incr: 256.
   [stream atEnd not] whileTrue: [
      idx := stream next asInteger.
      freqTable incr: idx.
   ].
   stream close.

   "Create leafs and add to priority queue"
   sortcol := SortedCollection new.
   d_opt ifTrue: [stdout << '----FREQUENCY TABLE----' << nl.].
   freqTable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
            "line below is for debugging"
            d_opt ifTrue: [stdout << 'freq[' << key asCharacter visible 
                           << '|' << key << ']=' << value << nl.].
            leaf := Leaf new: key asCharacter count: value.
            sortcol add: leaf.
      ]
   ].

   "Remove two smallest elements from priority queue.
   Repeat until one element is left."
   [sortcol size > 1] whileTrue: [
      firstNode := sortcol removeFirst.
      secondNode := sortcol removeFirst.
      totalFreq := firstNode count + secondNode count.
      tree := Tree new: firstNode char count: totalFreq 
               left: firstNode right: secondNode.
      sortcol add: tree.
   ].

   "Create encoding table"
   encTable := ZeroArray new: 257.
   d_opt ifTrue: [stdout << '----DFS AFTER TREE EXTRACTION----' << nl.].
   root := sortcol removeFirst.
   root depthFirst: [:char :string |
      d_opt ifTrue: [stdout << '[' << char asCharacter visible << ' | ' 
                     << char asInteger << ']=' << string << nl.].
      encTable at: char asInteger put: string.
   ] prefix: ''.


   t_opt ifTrue: [ " the -t option is used "
      d_opt ifTrue: [stdout << '---- -t FLAG OUTPUT ----' << nl].
      freqTable keysAndValuesDo: [:key :value |
         (value notNil and: [value > 0]) ifTrue: [
            path := encTable at: key.
            ((key > 32) & (key < 127)) ifTrue: [ "char is printable"
               stdout << ' ' << key asCharacter << ' '.
            ] ifFalse: [ "char is a hex"
               size := (key printString: 16) size.
               (size = 1) ifTrue: [
                  stdout << 'x0' << (key printString: 16).
               ].
               (size = 3) ifTrue: [
                  stdout << 'EOF'.
               ].
               (size = 2) ifTrue: [
                  stdout << 'x' << (key printString: 16).
               ].
            ].
            stdout << (value printPaddedWith: Character space to: 8)
                     << '  ' << path << nl.
         ]
      ]
   ] ifFalse: [ " the -t option is not used"
      |outBytes|
      "Open file for streaming"

      writeBitOut := [:outfile|
         outBytes := OutBits new: outfile.

         root postOrder: [:char :bit |
            (bit = (0 bitAt: 1)) ifTrue: [ "the node is a leaf"
               "write out of the leaf 0 bit here"
               outBytes writeBit: (0 bitAt: 1).
               "if char is 0 (rare), print out 0 bit then the 0 byte"
               char asInteger = 0 ifTrue: [
                  1 to: 9 do: [:idx | outBytes writeBit: (0 bitAt: 1)].
               ].
               "if char is eof, print out of 1 bit then the 0 byte"
               char asInteger = 256 ifTrue: [
                  1 to: 8 do: [:idx | outBytes writeBit: (0 bitAt: 1)].
                  outBytes writeBit: (1 bitAt: 1)
               ].
               "if char is neither 0 nor eof"
               ((char asInteger > 0) & (char asInteger < 256)) 
               ifTrue: [
               1 to: 8 do: [:idx |
                  outBytes writeBit: (char asInteger bitAt: (9 - idx))
               ].]
            ] ifFalse: [ "internal node, write out the 1 bit"
               outBytes writeBit: (1 bitAt: 1).
            ].
         ].

         "end of tree reached, write out the eof 1 bit"
         outBytes writeBit: (1 bitAt: 1).

         stream := FileStream open: filein mode: FileStream read.
         [stream atEnd not] whileTrue: [
            key := stream next asInteger.
            path := encTable at: key.
            1 to: path size do: [:idx |
               ascii := path byteAt: idx.
               ascii = 48 ifTrue: [outBytes writeBit: (0 bitAt: 1)].
               ascii = 49 ifTrue: [outBytes writeBit: (1 bitAt: 1)].
            ].

         ].
         "write out the eof encoded path"
         eof_path := encTable at: 256.
         1 to: eof_path size do: [:idx |
            ascii := eof_path byteAt: idx.
            ascii = 48 ifTrue: [outBytes writeBit: (0 bitAt: 1)].
            ascii = 49 ifTrue: [outBytes writeBit: (1 bitAt: 1)].
         ].

         outBytes flushByte.
         stream close.
      ].

      hasFileOut ifTrue: [
         outfile := FileStream open: fileout mode: FileStream write.
         writeBitOut value: outfile.
         outfile close.
      ]
      ifFalse: [writeBitOut value: stdout.].

      d_opt ifTrue: [
         stdout << '---- DEBUG ENCODING OUTPUT ----' << nl.
         stdout << 'FILEOUT: ' << fileout << nl.

         stdout << '---- WRITE ENCODED TREE FIRST:' << nl.

         root postOrder: [:char :bit |
            (bit = (0 bitAt: 1)) ifTrue: [ "the node is a leaf"
               stdout << bit.
               char asInteger = 0 ifTrue: [stdout << '000000000'].
               char asInteger = 256 ifTrue: [stdout << '000000001'].
               ((char asInteger > 0) & (char asInteger < 256)) 
               ifTrue: [
                  1 to: 8 do: [:idx |
                     stdout << (char asInteger bitAt: (9 - idx)).
                  ].
               ].
            ] ifFalse: [ "internal node, write out the 1 bit"
               stdout << bit.
            ].
         ].


         "end of tree reached, write out the eof 1 bit"
         stdout << 'EOF Tree = ' << 1 << nl. 

            
         stdout << '---- NOW WRITE THE PATHS:' << nl.
         stream := FileStream open: filein mode: FileStream read.
         [stream atEnd not] whileTrue: [
            byte := stream next asInteger.
            path := encTable at: byte.
            stdout << path << ' '.

         ].
         "write out the eof encoded path"
         eof_path := encTable at: 256.
         stdout << eof_path << nl.

         stream close.
      ].

   ].
]

"-----------------------------------"
"---------- DECOMPRESSION ----------"
"-----------------------------------"
(u_opt) ifTrue: [
   |byte treeDone decodeDone|

   stream := FileStream open: filein mode: FileStream read.

   " ---- Reconstructing the Tree ---- "
   fileBits := BitStream new: stream.
   stack := OrderedCollection new.
   treeDone := false.

   [treeDone] whileFalse: [
      firstBit := fileBits nextBit.
      indicator := firstBit bitAt: 1.
      " stdout << 'Indicator: ' << indicator << nl. "
      char := 0.
      indicator = 0 ifTrue: [ "indicator is 0"
         1 to: 8 do: [:idx | "reconstruct the byte"
            bit := fileBits nextBit.
            " stdout << bit. "
            char := char bitAt: (9 - idx) put: bit.
         ].
         char = 0 ifTrue: [ "if byte is 0, read 1 more bit"
            bit := fileBits nextBit.
            bit = 1 ifTrue: [char := 256]
            ifFalse: [char := 0].
         ].

         "add byte to stack as a leaf"
         leaf := Leaf new: char asCharacter count: 1.
         stack addFirst: leaf.
         " stdout << ' ---> char: ' << char asInteger << nl. "

      ] ifFalse: [ "indicator is 1"
         stack size = 1 ifTrue: [ "end of tree reached"
            " stdout << 'ending loop' << nl. "
            treeDone := true.
         ] ifFalse: [
            " stdout << ' <- popping two nodes' << nl. "
            first := stack removeFirst.
            second := stack removeFirst.
            tree := Tree new: 0 asCharacter count: 1 
                              left: second right: first.
            stack addFirst: tree.
         ]
      ]
   ].

   " ---- Traversing Tree To Decode ---- "
   decodeDone := false.
   decodedTree := stack removeFirst.
   hasFileOut ifTrue: [ "set outfile to "
      outfile := FileStream open: fileout mode: FileStream write.
      outBytes := OutBits new: outfile.
   ] ifFalse: [
      outBytes := OutBits new: stdout.
   ].

   [decodeDone] whileFalse: [
      " [decodeDone] whileFalse: [ "
      |mainPtr|
      mainPtr := decodedTree.
      " stdout << 'is ptr a leaf? --> ' << mainPtr isLeaf << nl. "
      [mainPtr isLeaf] whileFalse: [
         bit := fileBits nextBit.
         bit = 0 ifTrue: [mainPtr := mainPtr getLeft].
         bit = 1 ifTrue: [mainPtr := mainPtr getRight].
      ].
      

      (mainPtr char asInteger) = 256 ifTrue: [decodeDone := true]
      ifFalse: [ "write to stdout or file"
         byte := (mainPtr char) asInteger.

         1 to: 8 do: [:idx |
            outBytes writeBit: (byte bitAt: (9 - idx)). 
         ].
      ]
   ].

   hasFileOut ifTrue: [outfile close]
]

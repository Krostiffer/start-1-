#NAME
decode

#FUNCS
huffmanDecode' tree alphabet input = output

#VARS
tree     :: (Tree String)
alphabet :: String  -- code alphabet 
input    :: String  -- input stream

cur      :: Char    -- current character of input in stream
buffer   :: Char    -- written to output stream in o_flush 

output   :: String  -- output stream

--own variables can be added here


#PREDS
p_endOfStream = isEmpty input --do not modify

#OPS
o_init:
  output' = ""
  --further initialization assignments can be added here 

o_next: --read next character from stream; do not modify
  input' = removeFirst input
  cur' = first input
  
o_flush: --write contents of buffer to output; do not modify 
  output' = append buffer output


#FLOW

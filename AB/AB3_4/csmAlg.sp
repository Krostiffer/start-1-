#NAME 
csmAlg

#FUNCS
csm' x = r

#VARS 
x  :: CSMInstance
ca :: Int 
cb :: Int
oa :: Int
ob :: Int
l  :: Int
r  :: Bool

#PREDS
-- strlena and strlenb are called with (coa/cob -1) as the max index is at the (length -1)
p_eqlen = (strlena x ((coa x) -1)) == (strlenb x ((cob x) -1)) --true iff (length (concat a)) == (length (concat b))
p_ss    = (ssc x ca cb oa ob l)
p_aleq  = (((lena x ca) - oa) <= ((lenb x cb) - ob)) 
p_fc    = ((ca == ((coa x) - 1)) && (cb == ((cob x) - 1))) --fc = final call (last comparison)
p_eob   = ((lenb x cb) == (l + ob)) --end of b reached
p_empty = (strlena x ((coa x) -1)) == 0 --true iff (length (concat a)) == 0

#OPS
o_init:  
  ca' = 0
  cb' = 0
  oa' = 0
  ob' = 0

-- the length of the current String of a starting at the current offset (0 at the init, modified if ss was called before with b[x] as the smaller String)
o_alen:
  l' = ((lena x ca) - oa)

-- see o_alen
o_blen:
  l' = ((lenb x cb) - ob) 

-- in the last ssc call a[x] was smaller, thus a[x+1] and b[y] stays the same with added offset   
o_anbs: 
  ca' = (ca + 1)
  oa' = 0
  ob' = l

-- in the last ssc call b[x] was smaller, thus b[x+1] abd a[y] stays the same with added offset
o_asbn: 
  cb' = (cb + 1)
  ob' = 0
  oa' = l
    
-- in the last ssc call a[x] and a[y] were of the same length, thus both have to be increased to the next String a[x+1] b[y+1]   
o_anbn: 
  ca' = (ca + 1)
  cb' = (cb + 1)
  oa' = 0
  ob' = 0

o_yes:
  r' = True

o_no:
  r' = False
    
o_nop:

#FLOW
o_init = (p_eqlen (p_empty o_yes o_nop) o_no) -- if both are empty == TRUE if not == FALSE if a and b are the same size start alg
o_nop  = (p_aleq o_alen o_blen) -- if a[x] > b[y] choose b as the limiter, a otherwise
o_alen = (p_ss (p_fc o_yes (p_eob o_anbn o_anbs) ) o_no) -- check if a[x] and b[y] were the same size or a was smaller, also check final call is reached
o_blen = (p_ss o_asbn o_no)
o_anbs = o_nop
o_asbn = o_nop
o_anbn = o_nop
o_yes  = HALT
o_no   = HALT

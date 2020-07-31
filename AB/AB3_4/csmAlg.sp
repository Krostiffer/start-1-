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
p_eqlen = (strlena x (coa x) -1) == (strlenb x (cob x) -1) --true iff (length (concat a)) == (length (concat b))
p_ss    = (ssc x ca cb oa ob l)
p_aleq  = (((lena x ca) - oa) <= ((lenb x cb) - ob)) 
p_fc    = ((ca == ((coa x) - 1)) && (cb == ((cob x) - 1))) --fc = final call (last comparison)
p_eob   = ((lenb x cb) == (l + ob)) --end of b reached
p_empty = (strlena x (coa x) -1) == 0 --true iff (length (concat a)) == 0

#OPS
o_init:  
  ca' = 0
  cb' = 0
  oa' = 0
  ob' = 0

o_alen:
  l' = ((lena x ca) - oa)

o_blen:
  l' = ((lenb x cb) - ob) 
    
o_anbs: 
  ca' = (ca + 1)
  oa' = l
  ob' = 1

o_asbn: 
  cb' = (cb + 1)
  ob' = l
  oa' = 0
    
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
o_init = (p_eqlen (p_empty o_yes o_nop) o_no)
o_nop  = (p_aleq o_alen o_blen)
o_alen = (p_ss (p_fc o_yes (p_eob o_anbn o_anbs) ) o_no) --hint: look at AB3_4/reference/1.csv; predicate sequences = path through tree
o_blen = (p_ss o_asbn o_no)
o_anbs = o_nop
o_asbn = o_nop
o_anbn = o_nop
o_yes  = HALT
o_no   = HALT

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
p_eqlen = ? --true iff (length (concat a)) == (length (concat b))
p_ss    = (ssc x ca cb oa ob l)
p_aleq  = (((lena x ca) - oa) <= ((lenb x cb) - ob)) 
p_fc    = ((ca == ((coa x) - 1)) && (cb == ((cob x) - 1))) --fc = final call (last comparison)
p_eob   = ((lenb x cb) == (l + ob)) --end of b reached
p_empty = ? --true iff (length (concat a)) == 0

#OPS
o_init:  
  ca' = 0
  cb' = 0
  oa' = 0
  ob' = 0

o_alen:
  l' = ?

o_blen:
  l' = ?
    
o_anbs: 
  ca' = ?
  oa' = ?
  ob' = ?

o_asbn: 
  cb' = ?
  ob' = ?
  oa' = ?
    
o_anbn: 
  ca' = ?
  cb' = ?
  oa' = ?
  ob' = ?

o_yes:
  r' = ?

o_no:
  r' = ?
    
o_nop:

#FLOW
o_init = ?
o_nop  = ?
o_alen = (p_ss (? o_yes (? ? ?) ) o_no) --hint: look at AB3_4/reference/1.csv; predicate sequences = path through tree
o_blen = ?
o_anbs = ? 
o_asbn = ?
o_anbn = ?
o_yes  = HALT
o_no   = HALT

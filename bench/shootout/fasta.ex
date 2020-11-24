-- The Computer Language Shootout Benchmarks
--  http://shootout.alioth.debian.org/
-- 
--  Converted to Euphoria by Jason Gade
--  run: exu fasta.ex [N=1000]
--/**/with console

--atom t t=time()
constant r={{
">ONE Homo sapiens alu\n",
"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGA\n",
"TCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT\n",
"AAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAG\n",
"GCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCG\n",
"CCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAGGCCGGGCGCGGT\n",
"GGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCA\n",
"GGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAA\n",
"TTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAG\n",
"AATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCA\n",
"GCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAGGCCGGGCGCGGTGGCTCACGCCTGT\n",
"AATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACC\n",
"AGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTG\n",
"GTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACC\n",
"CGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAG\n",
"AGCGAGACTCCGTCTCAAAAAGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTT\n",
"TGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACA\n",
"TGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCT\n",
"GTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGG\n",
"TTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGT\n",
"CTCAAAAAGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGG\n",
"CGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCG\n",
"TCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTA\n",
--"TCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTA\n"
"CTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCG\n",
"AGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAGGCCG\n",
"GGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACC\n",
"TGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAA\n",
"TACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGA\n",
"GGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACT\n",
"GCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAGGCCGGGCGCGGTGGCTC\n",
"ACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGT\n",
"TCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGC\n",
"CGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCG\n",
"CTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTG\n",
"GGCGACAGAGCGAGACTCCG\n"},
{">TWO IUB ambiguity codes\n",
"cttBtatcatatgctaKggNcataaaSatgtaaaDcDRtBggDtctttataattcBgtcg\n",
"tactDtDagcctatttSVHtHttKtgtHMaSattgWaHKHttttagacatWatgtRgaaa\n",
"NtactMcSMtYtcMgRtacttctWBacgaaatatagScDtttgaagacacatagtVgYgt\n",
"cattHWtMMWcStgttaggKtSgaYaaccWStcgBttgcgaMttBYatcWtgacaYcaga\n",
"gtaBDtRacttttcWatMttDBcatWtatcttactaBgaYtcttgttttttttYaaScYa\n",
"HgtgttNtSatcMtcVaaaStccRcctDaataataStcYtRDSaMtDttgttSagtRRca\n",
"tttHatSttMtWgtcgtatSSagactYaaattcaMtWatttaSgYttaRgKaRtccactt\n",
"tattRggaMcDaWaWagttttgacatgttctacaaaRaatataataaMttcgDacgaSSt\n",
"acaStYRctVaNMtMgtaggcKatcttttattaaaaagVWaHKYagtttttatttaacct\n",
"tacgtVtcVaattVMBcttaMtttaStgacttagattWWacVtgWYagWVRctDattBYt\n",
--"tacgtVtcVaattVMBcttaMtttaStgacttagattWWacVtgWYagWVRctDattBYt\n"
"gtttaagaagattattgacVatMaacattVctgtBSgaVtgWWggaKHaatKWcBScSWa\n",
"accRVacacaaactaccScattRatatKVtactatatttHttaagtttSKtRtacaaagt\n",
"RDttcaaaaWgcacatWaDgtDKacgaacaattacaRNWaatHtttStgttattaaMtgt\n",
"tgDcgtMgcatBtgcttcgcgaDWgagctgcgaggggVtaaScNatttacttaatgacag\n",
"cccccacatYScaMgtaggtYaNgttctgaMaacNaMRaacaaacaKctacatagYWctg\n",
"ttWaaataaaataRattagHacacaagcgKatacBttRttaagtatttccgatctHSaat\n",
"actcNttMaagtattMtgRtgaMgcataatHcMtaBSaRattagttgatHtMttaaKagg\n",
"YtaaBataSaVatactWtataVWgKgttaaaacagtgcgRatatacatVtHRtVYataSa\n",
"KtWaStVcNKHKttactatccctcatgWHatWaRcttactaggatctataDtDHBttata\n",
"aaaHgtacVtagaYttYaKcctattcttcttaataNDaaggaaaDYgcggctaaWSctBa\n",
"aNtgctggMBaKctaMVKagBaactaWaDaMaccYVtNtaHtVWtKgRtcaaNtYaNacg\n",
"gtttNattgVtttctgtBaWgtaattcaagtcaVWtactNggattctttaYtaaagccgc\n",
"tcttagHVggaYtgtNcDaVagctctctKgacgtatagYcctRYHDtgBattDaaDgccK\n",
"tcHaaStttMcctagtattgcRgWBaVatHaaaataYtgtttagMDMRtaataaggatMt\n",
"ttctWgtNtgtgaaaaMaatatRtttMtDgHHtgtcattttcWattRSHcVagaagtacg\n",
"ggtaKVattKYagactNaatgtttgKMMgYNtcccgSKttctaStatatNVataYHgtNa\n",
"BKRgNacaactgatttcctttaNcgatttctctataScaHtataRagtcRVttacDSDtt\n",
"aRtSatacHgtSKacYagttMHtWataggatgactNtatSaNctataVtttRNKtgRacc\n",
"tttYtatgttactttttcctttaaacatacaHactMacacggtWataMtBVacRaSaatc\n",
"cgtaBVttccagccBcttaRKtgtgcctttttRtgtcagcRttKtaaacKtaaatctcac\n",
"aattgcaNtSBaaccgggttattaaBcKatDagttactcttcattVtttHaaggctKKga\n",
"tacatcBggScagtVcacattttgaHaDSgHatRMaHWggtatatRgccDttcgtatcga\n",
"aacaHtaagttaRatgaVacttagattVKtaaYttaaatcaNatccRttRRaMScNaaaD\n",
--"aacaHtaagttaRatgaVacttagattVKtaaYttaaatcaNatccRttRRaMScNaaaD\n"
"gttVHWgtcHaaHgacVaWtgttScactaagSgttatcttagggDtaccagWattWtRtg\n",
"ttHWHacgattBtgVcaYatcggttgagKcWtKKcaVtgaYgWctgYggVctgtHgaNcV\n",
"taBtWaaYatcDRaaRtSctgaHaYRttagatMatgcatttNattaDttaattgttctaa\n",
"ccctcccctagaWBtttHtBccttagaVaatMcBHagaVcWcagBVttcBtaYMccagat\n",
"gaaaaHctctaacgttagNWRtcggattNatcRaNHttcagtKttttgWatWttcSaNgg\n",
"gaWtactKKMaacatKatacNattgctWtatctaVgagctatgtRaHtYcWcttagccaa\n",
"tYttWttaWSSttaHcaaaaagVacVgtaVaRMgattaVcDactttcHHggHRtgNcctt\n",
"tYatcatKgctcctctatVcaaaaKaaaagtatatctgMtWtaaaacaStttMtcgactt\n",
"taSatcgDataaactaaacaagtaaVctaggaSccaatMVtaaSKNVattttgHccatca\n",
"cBVctgcaVatVttRtactgtVcaattHgtaaattaaattttYtatattaaRSgYtgBag\n",
"aHSBDgtagcacRHtYcBgtcacttacactaYcgctWtattgSHtSatcataaatataHt\n",
"cgtYaaMNgBaatttaRgaMaatatttBtttaaaHHKaatctgatWatYaacttMctctt\n",
"ttVctagctDaaagtaVaKaKRtaacBgtatccaaccactHHaagaagaaggaNaaatBW\n",
"attccgStaMSaMatBttgcatgRSacgttVVtaaDMtcSgVatWcaSatcttttVatag\n",
"ttactttacgatcaccNtaDVgSRcgVcgtgaacgaNtaNatatagtHtMgtHcMtagaa\n",
"attBgtataRaaaacaYKgtRccYtatgaagtaataKgtaaMttgaaRVatgcagaKStc\n",
"tHNaaatctBBtcttaYaBWHgtVtgacagcaRcataWctcaBcYacYgatDgtDHccta\n"}
,{">THREE Homo sapiens frequency\n",
"aacacttcaccaggtatcgtgaaggctcaagattacccagagaacctttgcaatataaga\n",
"atatgtatgcagcattaccctaagtaattatattctttttctgactcaaagtgacaagcc\n",
"ctagtgtatattaaatcggtatatttgggaaattcctcaaactatcctaatcaggtagcc\n",
"atgaaagtgatcaaaaaagttcgtacttataccatacatgaattctggccaagtaaaaaa\n",
"tagattgcgcaaaattcgtaccttaagtctctcgccaagatattaggatcctattactca\n",
--"tagattgcgcaaaattcgtaccttaagtctctcgccaagatattaggatcctattactca\n"
"tatcgtgtttttctttattgccgccatccccggagtatctcacccatccttctcttaaag\n",
"gcctaatattacctatgcaaataaacatatattgttgaaaattgagaacctgatcgtgat\n",
"tcttatgtgtaccatatgtatagtaatcacgcgactatatagtgctttagtatcgcccgt\n",
"gggtgagtgaatattctgggctagcgtgagatagtttcttgtcctaatatttttcagatc\n",
"gaatagcttctatttttgtgtttattgacatatgtcgaaactccttactcagtgaaagtc\n",
"atgaccagatccacgaacaatcttcggaatcagtctcgttttacggcggaatcttgagtc\n",
"taacttatatcccgtcgcttactttctaacaccccttatgtatttttaaaattacgttta\n",
"ttcgaacgtacttggcggaagcgttattttttgaagtaagttacattgggcagactcttg\n",
"acattttcgatacgactttctttcatccatcacaggactcgttcgtattgatatcagaag\n",
"ctcgtgatgattagttgtcttctttaccaatactttgaggcctattctgcgaaatttttg\n",
"ttgccctgcgaacttcacataccaaggaacacctcgcaacatgccttcatatccatcgtt\n",
"cattgtaattcttacacaatgaatcctaagtaattacatccctgcgtaaaagatggtagg\n",
"ggcactgaggatatattaccaagcatttagttatgagtaatcagcaatgtttcttgtatt\n",
"aagttctctaaaatagttacatcgtaatgttatctcgggttccgcgaataaacgagatag\n",
"attcattatatatggccctaagcaaaaacctcctcgtattctgttggtaattagaatcac\n",
"acaatacgggttgagatattaattatttgtagtacgaagagatataaaaagatgaacaat\n",
"tactcaagtcaagatgtatacgggatttataataaaaatcgggtagagatctgctttgca\n",
"attcagacgtgccactaaatcgtaatatgtcgcgttacatcagaaagggtaactattatt\n",
"aattaataaagggcttaatcactacatattagatcttatccgatagtcttatctattcgt\n",
"tgtatttttaagcggttctaattcagtcattatatcagtgctccgagttctttattattg\n",
"ttttaaggatgacaaaatgcctcttgttataacgctgggagaagcagactaagagtcgga\n",
"gcagttggtagaatgaggctgcaaaagacggtctcgacgaatggacagactttactaaac\n",
"caatgaaagacagaagtagagcaaagtctgaagtggtatcagcttaattatgacaaccct\n",
--"caatgaaagacagaagtagagcaaagtctgaagtggtatcagcttaattatgacaaccct\n"
"taatacttccctttcgccgaatactggcgtggaaaggttttaaaagtcgaagtagttaga\n",
"ggcatctctcgctcataaataggtagactactcgcaatccaatgtgactatgtaatactg\n",
"ggaacatcagtccgcgatgcagcgtgtttatcaaccgtccccactcgcctggggagacat\n",
"gagaccacccccgtggggattattagtccgcagtaatcgactcttgacaatccttttcga\n",
"ttatgtcatagcaatttacgacagttcagcgaagtgactactcggcgaaatggtattact\n",
"aaagcattcgaacccacatgaatgtgattcttggcaatttctaatccactaaagcttttc\n",
"cgttgaatctggttgtagatatttatataagttcactaattaagatcacggtagtatatt\n",
"gatagtgatgtctttgcaagaggttggccgaggaatttacggattctctattgatacaat\n",
"ttgtctggcttataactcttaaggctgaaccaggcgtttttagacgacttgatcagctgt\n",
"tagaatggtttggactccctctttcatgtcagtaacatttcagccgttattgttacgata\n",
"tgcttgaacaatattgatctaccacacacccatagtatattttataggtcatgctgttac\n",
"ctacgagcatggtattccacttcccattcaatgagtattcaacatcactagcctcagaga\n",
"tgatgacccacctctaataacgtcacgttgcggccatgtgaaacctgaacttgagtagac\n",
"gatatcaagcgctttaaattgcatataacatttgagggtaaagctaagcggatgctttat\n",
"ataatcaatactcaataataagatttgattgcattttagagttatgacacgacatagttc\n",
"actaacgagttactattcccagatctagactgaagtactgatcgagacgatccttacgtc\n",
"gatgatcgttagttatcgacttaggtcgggtctctagcggtattggtacttaaccggaca\n",
"ctatactaataacccatgatcaaagcataacagaatacagacgataatttcgccaacata\n",
"tatgtacagaccccaagcatgagaagctcattgaaagctatcattgaagtcccgctcaca\n",
"atgtgtcttttccagacggtttaactggttcccgggagtcctggagtttcgacttacata\n",
"aatggaaacaatgtattttgctaatttatctatagcgtcatttggaccaatacagaatat\n",
"tatgttgcctagtaatccactataacccgcaagtgctgatagaaaatttttagacgattt\n",
"ataaatgccccaagtatccctcccgtgaatcctccgttatactaattagtattcgttcat\n",
--"ataaatgccccaagtatccctcccgtgaatcctccgttatactaattagtattcgttcat\n"
"acgtataccgcgcatatatgaacatttggcgataaggcgcgtgaattgttacgtgacaga\n",
"gatagcagtttcttgtgatatggttaacagacgtacatgaagggaaactttatatctata\n",
"gtgatgcttccgtagaaataccgccactggtctgccaatgatgaagtatgtagctttagg\n",
"tttgtactatgaggctttcgtttgtttgcagagtataacagttgcgagtgaaaaaccgac\n",
"gaatttatactaatacgctttcactattggctacaaaatagggaagagtttcaatcatga\n",
"gagggagtatatggatgctttgtagctaaaggtagaacgtatgtatatgctgccgttcat\n",
"tcttgaaagatacataagcgataagttacgacaattataagcaacatccctaccttcgta\n",
"acgatttcactgttactgcgcttgaaatacactatggggctattggcggagagaagcaga\n",
"tcgcgccgagcatatacgagacctataatgttgatgatagagaaggcgtctgaattgata\n",
"catcgaagtacactttctttcgtagtatctctcgtcctctttctatctccggacacaaga\n",
"attaagttatatatatagagtcttaccaatcatgttgaatcctgattctcagagttcttt\n",
"ggcgggccttgtgatgactgagaaacaatgcaatattgctccaaatttcctaagcaaatt\n",
"ctcggttatgttatgttatcagcaaagcgttacgttatgttatttaaatctggaatgacg\n",
"gagcgaagttcttatgtcggtgtgggaataattcttttgaagacagcactccttaaataa\n",
"tatcgctccgtgtttgtatttatcgaatgggtctgtaaccttgcacaagcaaatcggtgg\n",
"tgtatatatcggataacaattaatacgatgttcatagtgacagtatactgatcgagtcct\n",
"ctaaagtcaattacctcacttaacaatctcattgatgttgtgtcattcccggtatcgccc\n",
"gtagtatgtgctctgattgaccgagtgtgaaccaaggaacatctactaatgcctttgtta\n",
"ggtaagatctctctgaattccttcgtgccaacttaaaacattatcaaaatttcttctact\n",
"tggattaactacttttacgagcatggcaaattcccctgtggaagacggttcattattatc\n",
"ggaaaccttatagaaattgcgtgttgactgaaattagatttttattgtaagagttgcatc\n",
"tttgcgattcctctggtctagcttccaatgaacagtcctcccttctattcgacatcgggt\n",
"ccttcgtacatgtctttgcgatgtaataattaggttcggagtgtggccttaatgggtgca\n",
"actaggaatacaacgcaaatttgctgacatgatagcaaatcggtatgccggcaccaaaac\n",
"gtgctccttgcttagcttgtgaatgagactcagtagttaaataaatccatatctgcaatc\n",
"gattccacaggtattgtccactatctttgaactactctaagagatacaagcttagctgag\n",
"accgaggtgtatatgactacgctgatatctgtaaggtaccaatgcaggcaaagtatgcga\n",
"gaagctaataccggctgtttccagctttataagattaaaatttggctgtcctggcggcct\n",
"cagaattgttctatcgtaatcagttggttcattaattagctaagtacgaggtacaactta\n",
"tctgtcccagaacagctccacaagtttttttacagccgaaacccctgtgtgaatcttaat\n",
"atccaagcgcgttatctgattagagtttacaactcagtattttatcagtacgttttgttt\n",
"ccaacattacccggtatgacaaaatgacgccacgtgtcgaataatggtctgaccaatgta\n",
"ggaagtgaaaagataaatat\n"}}


without warning
without type_check

--include get.e

constant ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"&
               "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"&
               "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"&
               "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"&
               "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"&
               "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"&
               "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

integer K, idx
procedure p(sequence x)
    if not equal(x,r[K][idx]) then ?9/0 end if
    idx+=1
--  puts(1,x)
end procedure

constant CODES = "acgtBDHKMNRSVWY"

constant IUB = {0.27, 0.12, 0.12, 0.27,  0.02, 0.02, 0.02, 0.02, 0.02,
                0.02, 0.02, 0.02, 0.02, 0.02, 0.02}

constant HomoSapiens = {0.3029549426680, 0.1979883004921,
                        0.1975473066391, 0.3015094502008}



constant IM = 139968
constant IA = 3877
constant IC = 29573

integer last last = 42

function gen_random(atom max)

    last = remainder(last * IA + IC, IM)
    return( max * last / IM )

end function -- gen_random



-- weighted selection from alphabet
function makeCumulative(sequence genelist)
    atom cp cp = 0.0

    for i = 1 to length(genelist) do
        cp += genelist[i]
        genelist[i] = cp
    end for

    return genelist

end function -- makeCumulative



function selectRandom(sequence genelist)
    
--  C version uses binary search, this works better than find().
    integer ix, lo, hi
    atom r
    
    r = gen_random(1)
    lo = 1
    hi = length(genelist)
    
    if r < genelist[1] then return CODES[1] end if
    
    while hi > lo + 1 do
        ix = floor((hi + lo) / 2)
        if r < genelist[ix] then hi = ix
        else lo = ix
        end if
    end while
    
    return CODES[hi]
    
end function -- selectRandom



-- Generate and write FASTA format
procedure makeRandomFasta(sequence id, sequence desc, sequence genelist, integer n, integer linesize)

    integer ix
    sequence pick 
    
--  puts(1, ">" & id & " " & desc & "\n" )
    p( ">" & id & " " & desc & "\n" )
    
    pick = repeat('\n', linesize)
    while n > 0 do
        ix = 1
        
        if n < linesize then
            linesize = n 
            pick = pick[1..linesize] -- resize
        end if
        
        while ix <= linesize do
            pick[ix] = selectRandom(genelist)
            ix += 1
        end while
        
--      puts(1, pick & '\n')
        p( pick & '\n')
        n -= linesize
    end while
    
end procedure -- makeRandomFasta



procedure makeRepeatFasta(sequence id, sequence desc, sequence s, integer n, integer linesize)
    integer posn, new_posn, s_len
    sequence line
    
    -- need a moving window to print 'linesize' at a time
    -- watch out for off-by-one errors
    posn = 1
    s_len = length(s)
    
--  puts(1, ">" & id & " " & desc & "\n" )
    p( ">" & id & " " & desc & "\n" )
    
    while n > 0 do
        if n < linesize then linesize = n end if
        
        new_posn = posn + linesize
        if new_posn <= s_len then
            line = s[posn..new_posn - 1]
            posn = new_posn
        else
            line = s[posn..length(s)]
            posn = new_posn - s_len
            line &= s[1..posn - 1]
        end if
        
--      puts(1, line & '\n')
        p( line & '\n')
        n -= linesize
        
    end while
        
end procedure -- makeRepeatFasta



constant LINE_LENGTH = 60

    object n
    sequence freq_IUB
    sequence freq_HomoSapiens

    n = 1000

for i=1 to 200 do
    last = 42
    freq_IUB = makeCumulative(IUB)
    freq_HomoSapiens = makeCumulative(HomoSapiens)
    
K=1 idx=1
    makeRepeatFasta("ONE", "Homo sapiens alu", ALU, n * 2, LINE_LENGTH)
K=2 idx=1
    makeRandomFasta("TWO", "IUB ambiguity codes", freq_IUB, n * 3, LINE_LENGTH)
K=3 idx=1
    makeRandomFasta("THREE", "Homo sapiens frequency", freq_HomoSapiens, n * 5, LINE_LENGTH)
end for
    
--printf(1,"%3.8f seconds\n",time()-t)
--if getc(0) then end if

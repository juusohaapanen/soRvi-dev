Known issues

* 0.1.44 HKK.R
    - MapInfo files downloaded from http://kartta.hel.fi/avoindata/aineistot/pk_seudun_aanestysalueet.zip
      for Helsinki are named 
        hki_äänestysalue_kkj2.DAT
        hki_äänestysalue_kkj2.ID
        hki_äänestysalue_kkj2.MAP
        hki_äänestysalue_kkj2.TAB

      -> 'ää' from the original zip file will cause problems on Linux, at least
         openSUSE 12.1 and Ubuntu 11.10 and 'ä' is not displayed right. This
         is likely an encoding issue and is not present on Windows 7. OSX is 
         untested. R will not be able to list this MapInfo file and thus it
         cannot be read in.

      Workaround: rename files (4)
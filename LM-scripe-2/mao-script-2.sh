docker run -v /home/mao/LM-scripe-2/cover/:/mao pep_cover:2 Rscript pep-cover.R
docker run --rm -v /home/mao/LM-scripe-2/kda-pepnum/:/mao kda_pep:2 Rscript mao-mw-kda.R
docker run --rm -v /home/mao/LM-scripe-2/kda-pepnum/:/mao kda_pep:2 Rscript mao-peptidenumber.R

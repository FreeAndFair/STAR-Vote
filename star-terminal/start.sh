#!/bin/sh

export STAR_TERMINAL_ID=9b59bbb0-5a45-11e4-8ed6-0800200c9a66
export STAR_PUBLIC_KEY=b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c
export STAR_INIT_PUBLIC_HASH=fcc578c708d198baecd7024be8dae72639d1560f3eaeac24f651e7be69c2b886
export STAR_INIT_INTERNAL_HASH=7ab9c86899a0007945ee13cd00b565b35d6dc3807ac9eaf08b9729b3a1267ffd
export STAR_PUBLIC_SALT=7ff1adc82440511d520c1116df34fee4e88003ccc3e73ac95f29958df06eefce
export STAR_POST_VOTE_URL=http://controller/votes

dist/build/star-terminal/star-terminal -p 8000 &

curl -X POST http://localhost:8000/ballots/oregon-2014/codes/25682 --data param=whatever

rm -r graphs
mkdir -p graphs

for i in {3..9}; do 
    CO=${i}co
    ../target/release/schnyderflip build $i graphs/n$i.flipgraph -t8
    ../target/release/schnyderflip build $i graphs/n${i}co.flipgraph -t8 -co
    echo "save -lew graphs/n${i}" > prog
    echo "stats --csv" >> prog
    echo "exit" >> prog
    cat prog | ../target/release/schnyderflip explore graphs/n$i.flipgraph > graphs/n${i}.stats

    echo "save -lew graphs/n${i}co" > prog
    echo "stats --csv" >> prog
    echo "exit" >> prog
    cat prog | ../target/release/schnyderflip explore graphs/n${i}co.flipgraph > graphs/n${i}co.stats
done

rm prog

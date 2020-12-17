mkdir -p graphs

for i in {3..10}; do 

    CO=${i}co
    rm graphs/n${i}.flipgraph 2> /dev/null
    ../target/release/schnyderflip build $i graphs/n$i.flipgraph -t8
    rm graphs/n${i}co.flipgraph 2> /dev/null
    ../target/release/schnyderflip build $i graphs/n${i}co.flipgraph -t8 -co
    echo "save -lew graphs/n${i}" > prog
    echo "stats --csv" >> prog
    echo "exit" >> prog
    rm graphs/n${i}.stats 2> /dev/null
    cat prog | ../target/release/schnyderflip explore graphs/n$i.flipgraph > graphs/n${i}.stats

    echo "save -lew graphs/n${i}co" > prog
    echo "stats --csv" >> prog
    echo "exit" >> prog
    rm graphs/n${i}co.stats 2> /dev/null
    cat prog | ../target/release/schnyderflip explore graphs/n${i}co.flipgraph > graphs/n${i}co.stats
done

rm prog

coisas que tornam o execOff:
    - return
    - break
    - continue

func y(...){ 
    return; -- so posso modificar o topo do loop control se ativacao tiver ok
}


func x(...){ -- chamou x (x: ativ 1)
    while(...){ -- esse while forma o topo da pilha loopControl
        y();-- chamou y, (y: ativ 2)
    }

    ERRO FATAL NAO DEVE SER EXECUTADO
}

[
    for1(){
        for2(){
            return;
        }
        -- exec mantem off -- vai tirar o fo2
        
    }-- vai tirar o for1 
]
    

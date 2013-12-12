buffer <- 1
active <- 2
loop 
{
    
    setactivepage(buffer);
    рисуем тута
    setvisualpage(buffer);
    swap(buffer, active);
}
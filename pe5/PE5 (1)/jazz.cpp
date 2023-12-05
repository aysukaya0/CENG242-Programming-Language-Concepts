#include "jazz.h"
#include "kpop.h"
#include "metal.h"
#include "rock.h"


int JazzBand::play(MusicBand *other)
{
    int score;
    KPopBand *kpop = dynamic_cast<KPopBand *>(other);
    if (kpop) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.5;
    }

    MetalBand *metal = dynamic_cast<MetalBand *>(other);
    if (metal) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*1.3;
    }

    RockBand *rock = dynamic_cast<RockBand *>(other);
    if (rock) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.7;
    }

    JazzBand *jazz = dynamic_cast<JazzBand *>(other);
    if (jazz) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.7;
    }
    set_energy(get_energy()-get_energy()*0.06);
    return score;
}

void JazzBand::rehearse(void) 
{
    set_energy(get_energy()-(0.5*0.06)*get_energy());
    set_talent(get_talent()+5);
}
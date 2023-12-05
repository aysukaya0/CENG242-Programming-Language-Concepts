#include "rock.h"
#include "kpop.h"
#include "metal.h"
#include "jazz.h"

int RockBand::play(MusicBand *other)
{
    int score;
    KPopBand *kpop = dynamic_cast<KPopBand *>(other);
    if (kpop) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.5;
    }

    MetalBand *metal = dynamic_cast<MetalBand *>(other);
    if (metal) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*1.4;
    }

    RockBand *rock = dynamic_cast<RockBand *>(other);
    if (rock) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*1.0;
    }

    JazzBand *jazz = dynamic_cast<JazzBand *>(other);
    if (jazz) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.8;
    }
    set_energy(get_energy()-get_energy()*0.1);
    return score;
}

void RockBand::rehearse(void) 
{
    set_energy(get_energy()-(0.5*0.1)*get_energy());
    set_talent(get_talent()+10);
}
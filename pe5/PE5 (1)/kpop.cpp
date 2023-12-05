#include "kpop.h"
#include "metal.h"
#include "rock.h"
#include "jazz.h"

int KPopBand::play(MusicBand *other)
{
    int score;
    KPopBand *kpop = dynamic_cast<KPopBand *>(other);
    if (kpop) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*2.0;
    }

    MetalBand *metal = dynamic_cast<MetalBand *>(other);
    if (metal) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.5;
    }

    RockBand *rock = dynamic_cast<RockBand *>(other);
    if (rock) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.5;
    }

    JazzBand *jazz = dynamic_cast<JazzBand *>(other);
    if (jazz) {
        score = (get_fan_count() + 0.1*get_talent()*get_energy())*0.5;
    }
    set_energy(get_energy()-get_energy()*0.2);
    return score;
}

void KPopBand::rehearse(void) 
{
    set_energy(get_energy()-(0.5*0.2)*get_energy());
}
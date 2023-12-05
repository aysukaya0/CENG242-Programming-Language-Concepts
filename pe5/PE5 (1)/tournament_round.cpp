#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() {}
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) { 
    bands = _bands;
}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) {
    std::list<MusicBand*> band_list(_bands.begin(),_bands.end());
    bands = band_list;
}

std::size_t TournamentRound::size() {
    return bands.size();
 }
    
//TournamentRound::TournamentRound(TournamentRound& other) { }
//TournamentRound::TournamentRound(TournamentRound&& other) { }
TournamentRound& TournamentRound::operator=(TournamentRound&& other) {
    if (this != &other) {
        bands.clear();
        bands = other.bands;
    }
    return *this;
}
TournamentRound& TournamentRound::get_next_round() {
    std::list<MusicBand *> band_list = bands;
    if (size() % 2 == 0) { //even number of bands
        std::list<MusicBand *> result;
        while (band_list.size() != 0) {
            MusicBand *first = band_list.front();
            MusicBand *second = band_list.back();
            int first_score = first->play(second);
            int second_score = second->play(first);
            if (first_score > second_score){
                int fan_change = first_score - second_score;
                if (fan_change <= second->get_fan_count()){
                    first->set_fan_count(first->get_fan_count()+fan_change);
                    second->set_fan_count(second->get_fan_count()-fan_change);
                }
                else {
                    first->set_fan_count(first->get_fan_count()+second->get_fan_count());
                    second->set_fan_count(0);
                }
                result.push_back(first);
            }
            else if (first_score == second_score){
                result.push_back(first);
            }
            else {
                int fan_change = second_score - first_score;
                if (fan_change <= first->get_fan_count()){
                    second->set_fan_count(second->get_fan_count()+fan_change);
                    first->set_fan_count(first->get_fan_count()-fan_change);
                }
                else {
                    second->set_fan_count(second->get_fan_count()+first->get_fan_count());
                    first->set_fan_count(0);
                }
                result.push_back(second);
            }
            band_list.pop_front();
            band_list.pop_back();
        }
        return *(new TournamentRound(result));
    } 

    else { //odd number of bands
        std::list<MusicBand *> result;
        while (band_list.size() != 1){
            MusicBand *first = band_list.front();
            MusicBand *second = band_list.back();
            int first_score = first->play(second);
            int second_score = second->play(first);
            if (first_score > second_score){
                int fan_change = first_score - second_score;
                if (fan_change <= second->get_fan_count()){
                    first->set_fan_count(first->get_fan_count()+fan_change);
                    second->set_fan_count(second->get_fan_count()-fan_change);
                }
                else {
                    first->set_fan_count(first->get_fan_count()+second->get_fan_count());
                    second->set_fan_count(0);
                }
                result.push_back(first);
            }
            else if (first_score == second_score){
                result.push_back(first);
            }
            else {
                int fan_change = second_score - first_score;
                if (fan_change <= first->get_fan_count()){
                    second->set_fan_count(second->get_fan_count()+fan_change);
                    first->set_fan_count(first->get_fan_count()-fan_change);
                }
                else {
                    second->set_fan_count(second->get_fan_count()+first->get_fan_count());
                    first->set_fan_count(0);
                }
                result.push_back(second);
            }
            band_list.pop_front();
            band_list.pop_back();
        }
        result.push_back(band_list.front());
        return *(new TournamentRound(result));
    } 
}

std::ostream& operator<< (std::ostream &os, TournamentRound &other) {
    std::list<MusicBand*>::iterator it = other.bands.begin();
    os << (*it)->get_name();
    it++;
    for (; it != other.bands.end(); it++){
        os << "\t" << (*it)->get_name();
    }
    return os; 
}
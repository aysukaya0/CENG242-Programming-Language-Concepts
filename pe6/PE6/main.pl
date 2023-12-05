:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

% DO NOT CHANGE THE UPPER CONTENT, WRITE YOUR CODE AFTER THIS LINE
is_vote_wasted(City, PoliticalParty) :-
    not((candidate(_, PoliticalParty, City, _), elected(City, PoliticalParty, _))).

is_candidate_elected(Name, PoliticalParty) :-
    candidate(Name, PoliticalParty, City, Row),
    elected(City, PoliticalParty, ElectedCount),
    Row =< ElectedCount.

candidate_count_from_city([], GivenCity, 0).
candidate_count_from_city([CandidateName | Rest], GivenCity, Count) :-
    not(candidate(CandidateName, _, GivenCity, _)),
    candidate_count_from_city(Rest, GivenCity, NewCount),
    Count is NewCount.
candidate_count_from_city([CandidateName | Rest], GivenCity, Count) :-
    candidate(CandidateName, _, GivenCity, _),
    candidate_count_from_city(Rest, GivenCity, NewCount),
    Count is NewCount + 1.
    
all_parties(ListOfPoliticalParties) :-
    findall(PoliticalParty, party(PoliticalParty, _), ListOfPoliticalParties).
    
all_candidates_from_party(PoliticalParty, ListOfCandidates) :-
    findall(Name, candidate(Name, PoliticalParty, _, _), ListOfCandidates).
    
all_elected_from_party(PoliticalParty, ListOfCandidates) :-
    findall(Name, is_candidate_elected(Name, PoliticalParty), ListOfCandidates).
    
election_rate(PoliticalParty, Percentage) :-
    all_elected_from_party(PoliticalParty, ListOfElectedes),
    length(ListOfElectedes, NumberOfElectedes),
    all_candidates_from_party(PoliticalParty, ListOfCandidates),
    length(ListOfCandidates, NumberOfCandidates),
    Percentage is (NumberOfElectedes / NumberOfCandidates).
    
council_percentage(PoliticalParty, Percentage) :-
    all_elected_from_party(PoliticalParty, ListOfElectedes),
    length(ListOfElectedes, NumberOfElectedes),
    to_elect(RepresentativeCount),
    Percentage is (NumberOfElectedes / RepresentativeCount).
    
alternative_debate_setups(DescriptionString, OrderedListOfCandidates) :-

    
    
    
    
    
    
    
    
    
    

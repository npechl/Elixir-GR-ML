import glob
import gzip
import os
import sys
import json
import csv
from datetime import datetime

#########################################

def create_csv_dialects():
    # Define CSV dialect to be used.
    csv.register_dialect(
        'writer_no_quotes',
        delimiter = '\t',
        lineterminator = '\n',
        quoting=csv.QUOTE_NONE,
        quotechar=''
    )

#########################################

def clean_newline_tab(field):
    if '\n' in field or '\r' in field:
        field = field.replace('\n', '').replace('\r', '')
    if '\t' in field:
        field = field.replace('\t', '')

    return field

#########################################

def get_openalex_id_suffix(value):
    id_prefix = 'https://openalex.org/'
    if not value.startswith(id_prefix):
        return ''

    return value.replace(id_prefix, '', 1)

#########################################

def get_str_clean(value):
    if not isinstance(value, str):
        return ''
    value = clean_newline_tab(value).strip()

    return value

#########################################

def get_year(value):
    if not isinstance(value, int):
        return ''

    max_accepted_year = int(datetime.now().year) + 3
    return '' if (value > max_accepted_year or value < 0) else str(value)

#########################################

def get_doi(doi_field):
    doi_prefix = 'https://doi.org/'
    doi_field = get_str_clean(doi_field).casefold()
    if not doi_field or not doi_field.startswith(doi_prefix + "10."):
        return ''

    return doi_field.replace(doi_prefix, '', 1)

#########################################

def get_pmid(ids_field):
    if not 'pmid' in ids_field:
        return ''

    pmid_prefix = 'https://pubmed.ncbi.nlm.nih.gov/'
    pmid_field = ids_field['pmid']
    if not pmid_field or not pmid_field.startswith(pmid_prefix):
        return ''

    pmid = pmid_field.replace(pmid_prefix, '', 1)
    return pmid if pmid.isdigit() else ''

#########################################

def get_authorships(authorships_list) -> list:
    '''
    Returns:
        - "" if no authorships countries were found
        - a comma separated string of countries sorte by name
    '''
    authorships_countries = []
    if not isinstance(authorships_list, list):
        return authorships_countries


    for authorship_dict in authorships_list:
        if 'countries' not in authorship_dict or not isinstance(authorship_dict['countries'], list):
            continue
        else:
            authorships_countries = list(set(authorships_countries) | set(authorship_dict['countries']))

    return ",".join(sorted(authorships_countries, key=str.lower))


#########################################

def get_concepts(concepts_list):

    concepts_ML_NLP_AI_id_score = []
    concepts_id_score = []
    concepts_L0 = False
    concepts_L1 = False
    if not isinstance(concepts_list, list):
        return concepts_id_score


    for concept_dict in concepts_list:
        concept_score_list = get_concept(concept_dict)
        if concept_score_list:
            # if not concepts_L0 and concept_score_list[1] == 0 and concept_score_list[2] in ["Medicine", "Biology"]:
            if not concepts_L0 and concept_score_list[0] in ["C71924100", "C86803240"]:
                    concepts_L0 = True
            # if not concepts_L1 and concept_score_list[1] == 1 and concept_score_list[2] in ["Artificial intelligence", "Machine learning", "Natural language processing"]:
            if not concepts_L1 and concept_score_list[0] in ["C154945302", "C119857082", "C204321447"]:
                    concepts_L1 = True
            # keep only concepts  ML_AI_NLP
            if concept_score_list[0] in ["C154945302", "C119857082", "C204321447"]:
                concepts_ML_NLP_AI_id_score.append(concept_score_list)
            # keep only level 2 concepts children of ML_AI_NLP
            if concept_score_list[0] in concepts_level_2_ML_AI_NLP:
                concepts_id_score.append(concept_score_list)

    if concepts_L0 and concepts_L1:
        return [sorted(concepts_ML_NLP_AI_id_score, key=lambda x: x[3]), sorted(concepts_id_score, key=lambda x: x[3])]
    else:
        return ""


#########################################

def get_concept(concept):

    max_concept_level = 2
    concept_id_score = []
    try:
        if concept['level'] > max_concept_level:
            return concept_id_score

        id_suffix = get_openalex_id_suffix(get_str_clean(concept['id']))
        level = concept['level']
        name = concept['display_name']
        score = float((concept['score']))
        if not (id_suffix and name):
            return concept_id_score
        concept_id_score.append(id_suffix)
        concept_id_score.append(level)
        concept_id_score.append(name)
        concept_id_score.append(score)
        return concept_id_score

    except Exception:
        return concept_id_score

#########################################

def parse_work_json(work_json):
    '''
    Return None or a dict {doi: [[Concept_id, Concept_Score],[],..}
    '''
    work = json.loads(work_json)


    if not ('concepts' in work):
        return None

    if 'doi' in work:
        work_doi = get_doi(work['doi'])
    else:
        work_doi = ''

    if 'ids' in work:
        work_pmid = get_pmid(work['ids'])
    else:
        work_pmid = ''

    if not work_doi and not work_pmid:
        return None


    work_concepts = get_concepts(work['concepts'])
    if work_concepts == "":
        return None

    if 'publication_year' in work:
        year = get_year(work['publication_year'])
    else:
        year = ''

    if 'authorships' in work:
        authorships = get_authorships(work['authorships'])
    else:
        authorships = ''


    return [work_doi, work_pmid, year, authorships, work_concepts]

##################################################################################

def main():
    start_time = datetime.now()
    if len(sys.argv) < 3:
        sys.exit("Usage: python3 script.py <path_to_works_folder> <output_file_gz>")
    if not sys.argv[2].endswith('.gz'):
        sys.exit("Usage: <output_file_gz> should end in .gz")

    # input
    works_folder = sys.argv[1]
    works_folder_files = os.path.join(works_folder, '*/*.gz')

    # output (in .gz)
    doi_to_concepts_file = sys.argv[2]

    create_csv_dialects()

    with gzip.open(doi_to_concepts_file, 'wt', newline='', compresslevel=6, encoding="utf-8") as doi_to_concepts_file:
        fout_writer = csv.writer(doi_to_concepts_file, dialect='writer_no_quotes')

        filelist = sorted(glob.glob(works_folder_files))
        for gzfile in filelist:
            print("=> Reading " + gzfile)

            with gzip.open(gzfile, 'rt', encoding="utf-8") as jsonl_file:
                for result in map(parse_work_json, jsonl_file):
                    if not result:
                        continue

                    doi, pmid, year, authorships, ids_scores_concept = result
                    ids_scores_concept_ML_AI_NLP, ids_scores_concept = ids_scores_concept
                    id_level_name_score_ML_AI_NLP = ",".join(["|".join(map(str, sublist)) for sublist in ids_scores_concept_ML_AI_NLP])
                    id_level_name_score = ",".join(["|".join(map(str, sublist)) for sublist in ids_scores_concept])


                    fout_writer.writerow([doi, pmid, year, authorships, id_level_name_score_ML_AI_NLP, id_level_name_score])

    print("=> Script Duration :",datetime.now() - start_time)

##################################################################################

if __name__== "__main__":

    with open('concepts_2-level_ML-AI-NLP-children.csv') as f:
        concepts_level_2_ML_AI_NLP = {x.rstrip(): "" for x in f}

    main()

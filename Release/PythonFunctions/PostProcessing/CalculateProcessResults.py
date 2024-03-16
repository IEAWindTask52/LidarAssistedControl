def CalculateProcessResults(frequency_results, statistics, post_processing_config):
    if 'CalculateProcessResults' not in post_processing_config or not post_processing_config['CalculateProcessResults']:
        return {}

    functions = post_processing_config['CalculateProcessResults']
    process_results = {}

    for this_function in functions:
        process_results = this_function(process_results, frequency_results, statistics)

    return process_results
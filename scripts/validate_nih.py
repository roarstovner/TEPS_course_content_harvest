#!/usr/bin/env python3
"""
NIH Validation Script
Validates the harvest pipeline for NIH (Norwegian School of Sport Sciences)
"""

import pickle
import urllib.request
import time
import re
from pathlib import Path

# Helper functions
def semester_to_url(semester):
    """Convert semester code to URL format"""
    semester_map = {
        1: 'var',  # Vår (Spring)
        3: 'host'  # Høst (Autumn)
    }
    return semester_map.get(semester, str(semester))

def add_course_url_nih(course_code, year, semester):
    """Generate NIH course URL"""
    sem_str = semester_to_url(semester)
    code_lower = course_code.lower()
    return f"https://www.nih.no/studier/emner/{year}/{sem_str}/{code_lower}.html"

def fetch_html(url):
    """Fetch HTML with user agent"""
    headers = {
        'User-Agent': 'Mozilla/5.0 (TEPS Research Project; +https://github.com/your-repo) Course Description Harvester'
    }
    try:
        req = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=30) as response:
            html = response.read().decode('utf-8')
            return {'html': html, 'success': True, 'error': None}
    except Exception as e:
        return {'html': None, 'success': False, 'error': str(e)}

def extract_fulltext_nih(html):
    """Simple extraction using known selector .fs-body"""
    if not html:
        return None

    # Very basic HTML parsing - look for .fs-body content
    # This is simplified; real extraction uses proper HTML parsing
    match = re.search(r'class="fs-body"[^>]*>(.*?)</div>', html, re.DOTALL)
    if match:
        content = match.group(1)
        # Strip HTML tags
        content = re.sub(r'<[^>]+>', ' ', content)
        # Clean whitespace
        content = re.sub(r'\s+', ' ', content).strip()
        return content
    return None

# Load test courses (simplified - using known structure from output)
test_courses = [
    {'year': 2021, 'sem': 3, 'code': 'LKI115-1', 'status': 2},
    {'year': 2021, 'sem': 3, 'code': 'LKI125-1', 'status': 2},
    {'year': 2022, 'sem': 3, 'code': 'LKI215-1', 'status': 2},
    {'year': 2023, 'sem': 1, 'code': 'LKI110-1', 'status': 1},
    {'year': 2023, 'sem': 1, 'code': 'LKI205-1', 'status': 1},
    {'year': 2023, 'sem': 1, 'code': 'LKI220-1', 'status': 1},
    {'year': 2024, 'sem': 1, 'code': 'LKI130-1', 'status': 1},
    {'year': 2024, 'sem': 1, 'code': 'LKI200-1', 'status': 1},
    {'year': 2024, 'sem': 1, 'code': 'LKI210-1', 'status': 1},
    {'year': 2024, 'sem': 1, 'code': 'LKI300-1', 'status': 1},
    {'year': 2024, 'sem': 3, 'code': 'LKI106-1', 'status': 1},
    {'year': 2024, 'sem': 3, 'code': 'LKI130-1', 'status': 1},
    {'year': 2024, 'sem': 3, 'code': 'LKI520-1', 'status': 1},
    {'year': 2025, 'sem': 1, 'code': 'LKI100-1', 'status': 1},
    {'year': 2025, 'sem': 1, 'code': 'LKI130-1', 'status': 1},
    {'year': 2025, 'sem': 1, 'code': 'LKI220-1', 'status': 1},
    {'year': 2025, 'sem': 1, 'code': 'LKI226-1', 'status': 1},
    {'year': 2025, 'sem': 3, 'code': 'LKI226-1', 'status': 1},
    {'year': 2025, 'sem': 3, 'code': 'LKI400-1', 'status': 1},
    {'year': 2025, 'sem': 3, 'code': 'LKI530-1', 'status': 2},
]

print('=== NIH Validation Pipeline ===\n')
print(f'Testing {len(test_courses)} courses\n')

# Step 1: Generate URLs
print('Step 1: Generating URLs...')
for course in test_courses:
    course['url'] = add_course_url_nih(course['code'], course['year'], course['sem'])

url_success = sum(1 for c in test_courses if c['url'])
print(f'URLs generated: {url_success}/{len(test_courses)}\n')
print('Sample URLs:')
for i in range(min(3, len(test_courses))):
    print(f'  {test_courses[i]["url"]}')
print()

# Step 2: Fetch HTML
print('Step 2: Fetching HTML...')
for i, course in enumerate(test_courses):
    print(f'  [{i+1:2d}/{len(test_courses):2d}] Fetching {course["code"]}...', end=' ')

    result = fetch_html(course['url'])
    course['html'] = result['html']
    course['html_success'] = result['success']
    course['html_error'] = result['error']

    if result['success']:
        print('OK')
    else:
        print(f'FAILED ({result["error"][:50]}...)')

    time.sleep(0.5)  # Be polite

fetch_success = sum(1 for c in test_courses if c['html_success'])
print(f'\nHTML fetch: {fetch_success}/{url_success}\n')

# Step 3: Verify course code in HTML
print('Step 3: Verifying page content...')
for course in test_courses:
    if course['html_success']:
        code_base = course['code'].split('-')[0]
        course['has_course_code'] = code_base.lower() in course['html'].lower()

verified = sum(1 for c in test_courses if c.get('has_course_code'))
print(f'Pages with course code: {verified}/{fetch_success}\n')

# Step 4: Extract fulltext
print('Step 4: Extracting text...')
for course in test_courses:
    if course['html_success']:
        course['extracted_text'] = extract_fulltext_nih(course['html'])
        course['text_length'] = len(course['extracted_text']) if course['extracted_text'] else 0

extraction_success = sum(1 for c in test_courses if c.get('extracted_text'))
print(f'Text extracted: {extraction_success}/{fetch_success}\n')

# Statistics
if extraction_success > 0:
    lengths = [c['text_length'] for c in test_courses if c.get('extracted_text')]
    print('Text length statistics:')
    print(f'  Min: {min(lengths)} characters')
    print(f'  Max: {max(lengths)} characters')
    print(f'  Mean: {int(sum(lengths)/len(lengths))} characters')
    print(f'  Median: {sorted(lengths)[len(lengths)//2]} characters')

# Summary
print('\n=== SUMMARY ===')
print(f'Total courses tested: {len(test_courses)}')
print(f'URL generation: {url_success}/{len(test_courses)} ({100*url_success//len(test_courses)}%)')
print(f'HTML fetch: {fetch_success}/{url_success} ({100*fetch_success//url_success if url_success > 0 else 0}%)')
print(f'Page verification: {verified}/{fetch_success} ({100*verified//fetch_success if fetch_success > 0 else 0}%)')
print(f'Text extraction: {extraction_success}/{fetch_success} ({100*extraction_success//fetch_success if fetch_success > 0 else 0}%)')

# Sample
if extraction_success > 0:
    sample = next(c for c in test_courses if c.get('extracted_text'))
    print('\n=== SAMPLE EXTRACTION ===')
    print(f'Course: {sample["code"]}')
    print(f'Text length: {sample["text_length"]} characters')
    print('First 500 characters:')
    print(sample['extracted_text'][:500])
    print('...\n')

# Save detailed results for later analysis
results_path = Path('data/nih_validation_results.txt')
with open(results_path, 'w') as f:
    f.write('NIH Validation Results\n')
    f.write('='*50 + '\n\n')
    for i, course in enumerate(test_courses):
        f.write(f'{i+1}. {course["code"]} ({course["year"]} Sem{course["sem"]})\n')
        f.write(f'   URL: {course["url"]}\n')
        f.write(f'   Fetch: {"OK" if course["html_success"] else "FAILED"}\n')
        if course.get('has_course_code'):
            f.write(f'   Course code found: YES\n')
        if course.get('extracted_text'):
            f.write(f'   Text extracted: {course["text_length"]} chars\n')
        if course.get('html_error'):
            f.write(f'   Error: {course["html_error"]}\n')
        f.write('\n')

print(f'\nDetailed results saved to {results_path}')
print('\nValidation complete!')

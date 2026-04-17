document.addEventListener('DOMContentLoaded', () => {
    const regexInput = document.getElementById('regex-input');
    const analyzeBtn = document.getElementById('analyze-btn');
    const resultDiv = document.getElementById('result');
    const exampleLinks = document.querySelectorAll('.example-link');

    // The rat object should be available globally after rat.js loads
    // Since js_of_ocaml export_all is used, it might be on window or a specific object.
    const analyzer = window; 

    function analyze() {
        const regex = regexInput.value;
        if (!regex) {
            resultDiv.style.display = 'none';
            return;
        }

        try {
            console.log('Analyzing regex:', regex);
            
            // Check if hasRedos exists on window
            if (typeof analyzer.hasRedos !== 'function') {
                console.error('Analyzer function hasRedos not found on window.');
                resultDiv.textContent = 'Error: Analyzer not loaded correctly.';
                resultDiv.className = 'error';
                return;
            }

            const result = analyzer.hasRedos(regex);
            console.log('Analysis result:', result);

            resultDiv.className = ''; // Reset classes
            if (result === 'Safe') {
                resultDiv.textContent = '✅ The regular expression is safe.';
                resultDiv.classList.add('safe');
            } else if (result === 'Dangerous') {
                resultDiv.textContent = '⚠️ The regular expression is dangerous (Exponential ReDoS detected).';
                resultDiv.classList.add('dangerous');
            } else {
                resultDiv.textContent = '❌ Parse Error: The regular expression could not be analyzed.';
                resultDiv.classList.add('error');
            }
        } catch (error) {
            console.error('Analysis error:', error);
            resultDiv.textContent = 'An unexpected error occurred during analysis.';
            resultDiv.className = 'error';
        }
    }

    analyzeBtn.addEventListener('click', analyze);
    
    regexInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            analyze();
        }
    });

    exampleLinks.forEach(link => {
        link.addEventListener('click', () => {
            regexInput.value = link.getAttribute('data-regex');
            analyze();
        });
    });
});

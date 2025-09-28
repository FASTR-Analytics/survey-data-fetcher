// ========================================
// PROGRESS BAR AND BUTTON CONTROL FUNCTIONS
// ========================================

// Handle progress updates from Shiny
Shiny.addCustomMessageHandler("updateProgress", function(data) {
    // Update progress bar
    const progressBar = document.getElementById("fetch_progress");
    const progressText = document.getElementById("progress_text");

    if (progressBar) {
        progressBar.style.width = data.percent + "%";
        progressBar.setAttribute("aria-valuenow", data.percent);
    }

    if (progressText && data.text) {
        progressText.textContent = data.text;
    }

    // Add animation class for visual feedback
    if (progressBar) {
        progressBar.classList.add("progress-animated");
        setTimeout(() => {
            progressBar.classList.remove("progress-animated");
        }, 300);
    }
});

// Disable fetch button during processing
Shiny.addCustomMessageHandler("disableButton", function(buttonId) {
    const button = document.getElementById(buttonId);
    if (button) {
        button.disabled = true;
        button.classList.add("loading");

        // Change text and icon to show loading state
        const originalHTML = button.innerHTML;
        button.setAttribute("data-original-html", originalHTML);
        button.innerHTML = '<i class="fa fa-spinner fa-spin"></i> Fetching...';
    }
});

// Re-enable fetch button when done
Shiny.addCustomMessageHandler("enableButton", function(buttonId) {
    const button = document.getElementById(buttonId);
    if (button) {
        button.disabled = false;
        button.classList.remove("loading");

        // Restore original button text and icon
        const originalHTML = button.getAttribute("data-original-html");
        if (originalHTML) {
            button.innerHTML = originalHTML;
        }
    }
});

// Initialize progress bar styling when page loads
document.addEventListener("DOMContentLoaded", function() {
    // Add smooth transitions to progress bars
    const progressBars = document.querySelectorAll(".progress-bar");
    progressBars.forEach(bar => {
        bar.style.transition = "width 0.3s ease";
    });

    // Add hover effects to fetch button
    const fetchButton = document.getElementById("fetch_data");
    if (fetchButton) {
        fetchButton.addEventListener("mouseenter", function() {
            if (!this.disabled) {
                this.style.transform = "translateY(-1px)";
                this.style.boxShadow = "0 4px 8px rgba(15, 112, 109, 0.3)";
            }
        });

        fetchButton.addEventListener("mouseleave", function() {
            if (!this.disabled) {
                this.style.transform = "translateY(0)";
                this.style.boxShadow = "none";
            }
        });
    }
});

// Auto-hide progress bar after successful completion
setTimeout(function() {
    const progressContainer = document.getElementById("progress_container");
    const progressBar = document.getElementById("fetch_progress");

    if (progressBar && progressBar.style.width === "100%") {
        setTimeout(() => {
            if (progressContainer) {
                progressContainer.style.opacity = "0.5";
            }
        }, 3000);
    }
}, 1000);
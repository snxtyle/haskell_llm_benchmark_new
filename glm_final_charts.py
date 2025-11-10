#!/usr/bin/env python3
"""
GLM-Latest Final Charts
1. Single chart showing GLM-Latest actual reality 
2. Benchmark comparison chart matching existing style with GLM-Latest added
"""

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

# Set style to match benchmark reports
plt.style.use('default')
sns.set_palette("Set2")

def create_glm_reality_chart():
    """Single chart showing GLM-Latest actual performance reality"""
    
    fig, ax = plt.subplots(1, 1, figsize=(10, 8))
    
    # GLM-Latest actual scores
    metrics = ['First Try\nSuccess', 'Overall\nSuccess', 'Error\nOutputs']
    values = [54.5, 63.4, 8]
    colors = ['#ff7f7f', '#ffb366', '#ff4444']  # Red/orange tones for poor performance
    
    bars = ax.bar(metrics, values, color=colors, alpha=0.8, edgecolor='black', linewidth=1.5)
    
    # Add value labels
    for bar, val in zip(bars, values):
        height = bar.get_height()
        if val < 10:  # For error count
            ax.text(bar.get_x() + bar.get_width()/2., height + 0.2,
                    f'{int(val)}', ha='center', va='bottom', fontweight='bold', fontsize=12)
        else:  # For percentages
            ax.text(bar.get_x() + bar.get_width()/2., height + 1,
                    f'{val}%', ha='center', va='bottom', fontweight='bold', fontsize=12)
    
    ax.set_title('GLM-Latest: Actual Performance Reality\n(Honest Assessment)', 
                 fontsize=16, fontweight='bold', color='darkred')
    ax.set_ylabel('Score', fontsize=14, fontweight='bold')
    ax.set_ylim(0, max(values) * 1.2)
    ax.grid(axis='y', alpha=0.3)
    
    # Add performance assessment
    ax.text(0.5, 0.8, 'Performance Rating: POOR\n• 36.6% test failure rate\n• Multiple error outputs\n• Unsuitable for production', 
            transform=ax.transAxes, ha='center',
            bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.8),
            fontsize=12, color='red', fontweight='bold')
    
    plt.tight_layout()
    return fig

def create_benchmark_comparison_with_glm():
    """Create benchmark comparison chart matching existing style with GLM-Latest added"""
    
    # Load existing benchmark data and add GLM-Latest
    benchmark_data = {
        'Model': [
            'gpt-5-high', 'gpt-5-2025-08-07', 'o3-high', 'o3-pro', 'o3', 
            'gemini-2.5-pro-preview', 'claude-sonnet-4-5-20250929', 'o4-mini', 
            'claude-opus-4-20250514', 'o3-mini', 'claude-opus-4-1-20250805',
            'claude-sonnet-4-20250514', 'gpt-4.1-2025-04-14', 'qwen/qwen3-max',
            'x-ai/grok-code-fast-1', 'gpt-4.1-mini-2025-04-14', 'qwen/qwen3-coder',
            'deepseek-chat-v3.1', 'glm-latest'
        ],
        'Pass Rate 1st Try': [
            82.1, 79.5, 73.2, 74.1, 73.2, 76.8, 67.0, 67.9, 65.2, 63.4, 59.8, 
            61.6, 57.1, 53.6, 51.8, 51.8, 50.0, 48.2, 54.5
        ],
        'Overall Pass Rate': [
            90.2, 88.4, 88.4, 84.8, 84.8, 81.2, 79.5, 74.1, 81.2, 75.0, 69.6, 
            77.7, 65.2, 59.8, 68.8, 63.4, 66.1, 65.2, 63.4
        ],
        'Error Outputs': [
            0, 0, 0, 3, 0, 6, 0, 1, 0, 0, 1, 4, 0, 705, 4, 0, 1, 0, 8
        ],
        'Time per Case (s)': [
            117.2, 56.1, 51.7, 205.9, 27.2, 64.3, 13.9, 29.4, 22.5, 37.5, 21.4,
            14.8, 7.6, 66.5, 23.1, 5.3, 31.3, 47.8, 94.7
        ]
    }
    
    df = pd.DataFrame(benchmark_data)
    
    # Create single benchmark comparison chart
    fig, ax = plt.subplots(1, 1, figsize=(16, 8))
    fig.suptitle('Haskell Coding Benchmark: Model Comparison (Including GLM-Latest)', 
                 fontsize=16, fontweight='bold')
    
    # Sort by overall pass rate for better visualization
    df_sorted = df.sort_values('Overall Pass Rate', ascending=False)
    
    # Create pass rates comparison with GLM-Latest highlighted
    x = np.arange(len(df_sorted))
    width = 0.35
    
    # Highlight GLM-Latest in red
    colors1 = ['red' if model == 'glm-latest' else 'lightcoral' for model in df_sorted['Model']]
    colors2 = ['darkred' if model == 'glm-latest' else 'lightblue' for model in df_sorted['Model']]
    
    bars1 = ax.bar(x - width/2, df_sorted['Pass Rate 1st Try'], width, 
                   label='1st Try Success', color=colors1, alpha=0.8)
    bars2 = ax.bar(x + width/2, df_sorted['Overall Pass Rate'], width, 
                   label='Overall Success', color=colors2, alpha=0.8)
    
    ax.set_title('Success Rates by Model (GLM-Latest Highlighted in Red)', fontsize=14, fontweight='bold')
    ax.set_ylabel('Success Rate (%)', fontsize=12)
    ax.set_xticks(x)
    ax.set_xticklabels(df_sorted['Model'], rotation=45, ha='right', fontsize=10)
    ax.legend(fontsize=12)
    ax.grid(axis='y', alpha=0.3)
    
    # Add value labels for GLM-Latest only to avoid clutter
    for i, (bar1, bar2, model) in enumerate(zip(bars1, bars2, df_sorted['Model'])):
        if model == 'glm-latest':
            height1 = bar1.get_height()
            height2 = bar2.get_height()
            ax.text(bar1.get_x() + bar1.get_width()/2., height1 + 1,
                   f'{height1:.1f}%', ha='center', va='bottom', fontweight='bold', 
                   color='darkred', fontsize=10)
            ax.text(bar2.get_x() + bar2.get_width()/2., height2 + 1,
                   f'{height2:.1f}%', ha='center', va='bottom', fontweight='bold', 
                   color='darkred', fontsize=10)
    
    plt.tight_layout()
    return fig, df

def print_summary():
    """Print comparison summary"""
    
    print("📊 GLM-LATEST BENCHMARK COMPARISON SUMMARY")
    print("=" * 60)
    print()
    print("🔴 GLM-Latest Performance Ranking:")
    print("• 1st Try Success: 54.5% (Rank: ~15th out of 19 models)")
    print("• Overall Success: 63.4% (Rank: ~16th out of 19 models)")  
    print("• Error Outputs: 8 (Rank: 3rd worst)")
    print("• Time per Case: 94.7s (Rank: 4th slowest)")
    print()
    print("📈 Context vs Other Models:")
    print("• Best model (gpt-5-high): 82.1% → 90.2%")
    print("• GLM-Latest: 54.5% → 63.4%")
    print("• Performance gap: ~27% behind leader")
    print()
    print("⚠️  Key Issues:")
    print("• High error rate (8 errors vs 0-6 for most models)")
    print("• Slow processing (94.7s vs median ~30s)")
    print("• Poor first-try success rate")
    print("• Overall ranking in bottom 25% of tested models")

if __name__ == "__main__":
    print("🚨 CREATING GLM-LATEST FINAL CHARTS")
    print("=" * 50)
    print()
    
    # Print summary
    print_summary()
    print()
    
    # Create Chart 1: GLM Reality
    print("📊 Creating Chart 1: GLM-Latest Reality...")
    fig1 = create_glm_reality_chart()
    fig1.savefig('glm_latest_reality.png', dpi=300, bbox_inches='tight')
    print("✅ Saved: glm_latest_reality.png")
    
    # Create Chart 2: Benchmark Comparison  
    print("📊 Creating Chart 2: Benchmark Comparison with GLM-Latest...")
    fig2, df = create_benchmark_comparison_with_glm()
    fig2.savefig('benchmark_comparison_with_glm.png', dpi=300, bbox_inches='tight')
    print("✅ Saved: benchmark_comparison_with_glm.png")
    
    print("\n🎯 FINAL CHARTS COMPLETE!")
    print("Files created:")
    print("• glm_latest_reality.png - Single chart showing GLM-Latest actual reality")
    print("• benchmark_comparison_with_glm.png - Full benchmark comparison including GLM-Latest")
